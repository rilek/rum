(ns rum.core
  (:refer-clojure :exclude [ref deref])
  (:require-macros rum.core)
  (:require
   [cljs.core]
   [cljsjs.react]
   [cljsjs.react.dom]
   [goog.object :as gobj]
   [goog.functions :as fns]
   [clojure.set :as set]
   [rum.specs]
   [daiquiri.core]
   [rum.cursor :as cursor]
   [rum.util :as util :refer [collect collect* call-all-2 call-all-3 call-all-4]]
   [rum.derived-atom :as derived-atom]))

(goog-define ^boolean USE_EFFECT? false)

(defn state
  "Given React component, returns Rum state associated with it."
  [^js/React.Component comp]
  (gobj/get (.-state comp) ":rum/state"))

(defn- extend! [obj props]
  (doseq [[k v] props
          :when (some? v)]
    (aset obj (name k) (clj->js v))))

(defn- build-class [render mixins display-name]
  (let [init           (collect   :init mixins)             ;; state props -> state
        before-render  (collect* [:will-mount
                                  :unsafe/will-mount
                                  :before-render] mixins)   ;; state -> state
        render         render                               ;; state -> [dom state]
        wrap-render    (collect   :wrap-render mixins)      ;; render-fn -> render-fn
        wrapped-render (reduce #(%2 %1) render wrap-render)
        did-mount      (collect* [:did-mount                ;; state -> state
                                  :after-render] mixins)    ;; state -> state
        will-remount    (collect* [:did-remount             ;; state -> state
                                   :will-remount] mixins)   ;; old-state state -> state
        should-update  (collect   :should-update mixins)    ;; old-state state -> boolean
        before-update  (collect* [:unsafe/will-update
                                  :unsafe/will-update
                                  :before-render] mixins)   ;; state -> state
        did-update     (collect* [:did-update               ;; state -> state
                                  :after-render] mixins)    ;; state -> state
        did-catch      (collect   :did-catch mixins)        ;; state error info -> state
        will-unmount   (collect   :will-unmount mixins)     ;; state -> state
        child-context  (collect   :child-context mixins)    ;; state -> child-context
        class-props    (reduce merge (collect :class-properties mixins))  ;; custom prototype properties and methods
        static-props   (reduce merge (collect :static-properties mixins)) ;; custom static properties and methods

        ctor           (fn [props]
                         (this-as this
                                  (aset this "state"
                                        #js {":rum/state"
                                             (-> (aget props ":rum/initial-state")
                                                 (assoc :rum/react-component this)
                                                 (call-all-3 init props)
                                                 volatile!)})
                                  (.call js/React.Component this props)))
        _              (goog/inherits ctor js/React.Component)
        prototype      (aget ctor "prototype")]

    (when-not (empty? before-render)
      (gobj/set prototype "UNSAFE_componentWillMount"
                (fn []
                  (this-as this
                           (vswap! (state this) call-all-2 before-render)))))

    (when-not (empty? did-mount)
      (aset prototype "componentDidMount"
            (fn []
              (this-as this
                       (vswap! (state this) call-all-2 did-mount)))))

    (aset prototype "componentWillReceiveProps"
          (fn [next-props]
            (this-as this
                     (let [old-state  @(state this)
                           state      (merge old-state
                                             (gobj/get next-props ":rum/initial-state"))
                           next-state (reduce #(%2 old-state %1) state will-remount)]
            ;; allocate new volatile so that we can access both old and new states in shouldComponentUpdate
                       (.setState ^js/React.Component this #js {":rum/state" (volatile! next-state)})))))

    (when-not (empty? should-update)
      (aset prototype "shouldComponentUpdate"
            (fn [_next-props next-state]
              (this-as this
                       (let [old-state @(state this)
                             new-state @(aget next-state ":rum/state")]
                         (or (some #(% old-state new-state) should-update) false))))))

    (when-not (empty? before-update)
      (gobj/set prototype "UNSAFE_componentWillUpdate"
                (fn [_ next-state]
                  (this-as this
                           (let [new-state (aget next-state ":rum/state")]
                             (vswap! new-state call-all-2 before-update))))))

    (aset prototype "render"
          (fn []
            (this-as this
                     (let [state (state this)
                           [dom next-state] (wrapped-render @state)]
                       (vreset! state next-state)
                       dom))))

    (when-not (empty? did-update)
      (aset prototype "componentDidUpdate"
            (fn [_ _]
              (this-as this
                       (vswap! (state this) call-all-2 did-update)))))

    (when-not (empty? did-catch)
      (aset prototype "componentDidCatch"
            (fn [error info]
              (this-as this
                       (vswap! (state this) call-all-4 did-catch error {:rum/component-stack (gobj/get info "componentStack")})
                       (.forceUpdate ^js/React.Component this)))))

    (aset prototype "componentWillUnmount"
          (fn []
            (this-as this
                     (when-not (empty? will-unmount)
                       (vswap! (state this) call-all-2 will-unmount))
                     (aset this ":rum/unmounted?" true))))

    (when-not (empty? child-context)
      (aset prototype "getChildContext"
            (fn []
              (this-as this
                       (let [state @(state this)]
                         (clj->js (transduce (map #(% state)) merge {} child-context)))))))

    (extend! prototype class-props)
    (aset ctor "displayName" display-name)
    (extend! ctor static-props)
    ctor))

(defn- set-meta! [c]
  (let [f #(let [^js ctr (c)]
             (.apply ctr ctr (js-arguments)))]
    (specify! f IMeta (-meta [_] (meta (c))))
    f))

(defn lazy-build
  "Wraps component construction in a way so that Google Closure Compiler
   can properly recognize and elide unused components. The extra `set-meta`
   fn is needed so that the compiler can properly detect that all functions
   are side effect free."
  [ctor render mixins display-name]
  (let [bf #(ctor render mixins display-name) ;; Avoid IIFE
        c  (fns/cacheReturnValue bf)]
    (set-meta! c)))

(defn- build-ctor [render mixins display-name]
  (let [class  (build-class render mixins display-name)
        key-fn (first (collect :key-fn mixins))
        ctor   (if (some? key-fn)
                 (fn [& args]
                   (let [props #js {":rum/initial-state" {:rum/args args}
                                    "key" (apply key-fn args)}]
                     (js/React.createElement class props)))
                 (fn [& args]
                   (let [props #js {":rum/initial-state" {:rum/args args}}]
                     (js/React.createElement class props))))]
    (with-meta ctor {:rum/class class})))

(declare static)

(defn- memo-compare-props [prev-props next-props]
  (= (aget prev-props ":rum/args")
     (aget next-props ":rum/args")))

(defn react-memo [f]
  (if-some [memo (.-memo js/React)]
    (memo f memo-compare-props)
    f))

(defn ^:no-doc build-defc [render-body mixins display-name]
  (cond
    (= mixins [static])
    (let [class (fn [props]
                  (apply render-body (aget props ":rum/args")))
          _     (aset class "displayName" display-name)
          memo-class (react-memo class)
          ctor  (fn [& args]
                  (.createElement js/React memo-class #js {":rum/args" args}))]
      (with-meta ctor {:rum/class memo-class}))

    (empty? mixins)
    (let [class (fn [props]
                  (apply render-body (aget props ":rum/args")))
          _     (aset class "displayName" display-name)
          ctor  (fn [& args]
                  (.createElement js/React class #js {":rum/args" args}))]
      (with-meta ctor {:rum/class class}))

    :else
    (let [render (fn [state] [(apply render-body (:rum/args state)) state])]
      (build-ctor render mixins display-name))))

(defn ^:no-doc build-defcs [render-body mixins display-name]
  (let [render (fn [state] [(apply render-body state (:rum/args state)) state])]
    (build-ctor render mixins display-name)))

(defn ^:no-doc build-defcc [render-body mixins display-name]
  (let [render (fn [state] [(apply render-body (:rum/react-component state) (:rum/args state)) state])]
    (build-ctor render mixins display-name)))

;; render queue


(def ^:private schedule
  (or (and (exists? js/window)
           (or js/window.requestAnimationFrame
               js/window.webkitRequestAnimationFrame
               js/window.mozRequestAnimationFrame
               js/window.msRequestAnimationFrame))
      #(js/setTimeout % 16)))

(def ^:private batch
  (or (when (exists? js/ReactNative) js/ReactNative.unstable_batchedUpdates)
      (when (exists? js/ReactDOM) js/ReactDOM.unstable_batchedUpdates)
      (fn [f a] (f a))))

(def ^:private empty-queue [])
(def ^:private render-queue (volatile! empty-queue))

(defn- render-one [comp]
  (when (and (some? comp) (not (gobj/get comp ":rum/unmounted?")))
    (.forceUpdate comp)))

(defn- render-all [queue]
  (run! render-one queue))

(defn- render []
  (let [queue @render-queue]
    (vreset! render-queue empty-queue)
    (batch render-all queue)))

(def ^:private sync-update? (volatile! false))

(defn request-render
  "Schedules react component to be rendered on next animation frame,
  unless the requested update happens to be in a sync-update phase."
  [component]
  (if @sync-update?
    (render-one component)
    (do
      (when (empty? @render-queue)
        (schedule render))
      (vswap! render-queue conj component))))

;; exporting to work around circular deps
(defn ^:export mark-sync-update [f]
  (if (fn? f)
    (fn wrapped-handler [e]
      (let [_ (vreset! sync-update? true)
            ret (f e)
            _ (vreset! sync-update? false)]
        ret))
    f))

(defn mount
  "Add element to the DOM tree. Idempotent. Subsequent mounts will just update element."
  [element node]
  (js/ReactDOM.render element node)
  nil)

(defn unmount
  "Removes component from the DOM tree."
  [node]
  (js/ReactDOM.unmountComponentAtNode node))

(defn hydrate
  "Same as [[mount]] but must be called on DOM tree already rendered by a server via [[render-html]]."
  [element node]
  (js/ReactDOM.hydrate element node))

(defn portal
  "Render `element` in a DOM `node` that is ouside of current DOM hierarchy."
  [element node]
  (js/ReactDOM.createPortal element node))

(defn create-context [default-value]
  (.createContext js/React default-value))


;; initialization


(defn with-key
  "Adds React key to element.
   
   ```
   (rum/defc label [text] [:div text])

   (-> (label)
       (rum/with-key \"abc\")
       (rum/mount js/document.body))
   ```"
  [element key]
  (js/React.cloneElement element #js {"key" key} nil))

(defn with-ref
  "Adds React ref (string or callback) to element.
   
   ```
   (rum/defc label [text] [:div text])

   (-> (label)
       (rum/with-ref \"abc\")
       (rum/mount js/document.body))
   ```"
  [element ref]
  (js/React.cloneElement element #js {"ref" ref} nil))

(defn dom-node
  "Usage of this function is discouraged. Use :ref callback instead.
  Given state, returns top-level DOM node of component. Call it during lifecycle callbacks. Can’t be called during render."
  [state]
  (js/ReactDOM.findDOMNode (:rum/react-component state)))

(defn ref
  "DEPRECATED: Use :ref (fn [dom-or-nil]) callback instead. See rum issue #124
  Given state and ref handle, returns React component."
  [state key]
  (-> state :rum/react-component (aget "refs") (aget (name key))))

(defn ref-node
  "DEPRECATED: Use :ref (fn [dom-or-nil]) callback instead. See rum issue #124
  Given state and ref handle, returns DOM node associated with ref."
  [state key]
  (js/ReactDOM.findDOMNode (ref state (name key))))


;; static mixin


(def static
  "Mixin. Will avoid re-render if none of component’s arguments have changed. Does equality check (`=`) on all arguments.
  
   ```
   (rum/defc label < rum/static
     [text]
     [:div text])
     
   (rum/mount (label \"abc\") js/document.body)

   ;; def != abc, will re-render
   (rum/mount (label \"def\") js/document.body)

   ;; def == def, won’t re-render
   (rum/mount (label \"def\") js/document.body)
   ```"
  {:should-update
   (fn [old-state new-state]
     (not= (:rum/args old-state) (:rum/args new-state)))})


;; local mixin


(defn local
  "Mixin constructor. Adds an atom to component’s state that can be used to keep stuff during component’s lifecycle. Component will be re-rendered if atom’s value changes. Atom is stored under user-provided key or under `:rum/local` by default.

      ```
      (rum/defcs counter < (rum/local 0 :cnt)
        [state label]
        (let [*cnt (:cnt state)]
          [:div {:on-click (fn [_] (swap! *cnt inc))}
            label @*cnt]))

      (rum/mount (counter \"Click count: \"))
      ```"
  ([initial] (local initial :rum/local))
  ([initial key]
   {:init
    (fn [state]
      (let [local-state (atom initial)
            ^js/React.Component component (:rum/react-component state)]
        (add-watch local-state key
                   (fn [_ _ p n]
                     (when (not= p n)
                       (request-render component))))
        (assoc state key local-state)))}))


;; reactive mixin


(def ^:private ^:dynamic *reactions*)

(def reactive
  "Mixin. Works in conjunction with [[react]].
  
   ```
   (rum/defc comp < rum/reactive
     [*counter]
     [:div (rum/react counter)])

   (def *counter (atom 0))
   (rum/mount (comp *counter) js/document.body)
   (swap! *counter inc) ;; will force comp to re-render
   ```"
  {:init
   (fn [state props]
     (assoc state :rum.reactive/key (random-uuid)))
   :wrap-render
   (fn [render-fn]
     (fn [state]
       (binding [*reactions* (volatile! #{})]
         (let [comp             (:rum/react-component state)
               old-reactions    (:rum.reactive/refs state #{})
               [dom next-state] (render-fn state)
               new-reactions    @*reactions*
               key              (:rum.reactive/key state)]
           (doseq [ref old-reactions]
             (when-not (contains? new-reactions ref)
               (remove-watch ref key)))
           (doseq [ref new-reactions]
             (when-not (contains? old-reactions ref)
               (add-watch ref key
                          (fn [_ _ p n]
                            (when (not= p n)
                              (request-render comp))))))
           [dom (assoc next-state :rum.reactive/refs new-reactions)]))))
   :will-unmount
   (fn [state]
     (let [key (:rum.reactive/key state)]
       (doseq [ref (:rum.reactive/refs state)]
         (remove-watch ref key)))
     (dissoc state :rum.reactive/refs :rum.reactive/key))})

(defn react
  "Works in conjunction with [[reactive]] mixin. Use this function instead of `deref` inside render, and your component will subscribe to changes happening to the derefed atom."
  [ref]
  (assert *reactions* "rum.core/react is only supported in conjunction with rum.core/reactive")
  (vswap! *reactions* conj ref)
  @ref)


;; derived-atom


(def ^{:style/indent 2
       :arglists '([refs key f] [refs key f opts])
       :doc "Use this to create “chains” and acyclic graphs of dependent atoms.
   
             [[derived-atom]] will:
          
             - Take N “source” refs.
             - Set up a watch on each of them.
             - Create “sink” atom.
             - When any of source refs changes:
                - re-run function `f`, passing N dereferenced values of source refs.
                - `reset!` result of `f` to the sink atom.
             - Return sink atom.

             Example:

             ```
             (def *a (atom 0))
             (def *b (atom 1))
             (def *x (derived-atom [*a *b] ::key
                       (fn [a b]
                         (str a \":\" b))))
             
             (type *x)  ;; => clojure.lang.Atom
             (deref *x) ;; => \"0:1\"
             
             (swap! *a inc)
             (deref *x) ;; => \"1:1\"
             
             (reset! *b 7)
             (deref *x) ;; => \"1:7\"
             ```

             Arguments:
          
             - `refs` - sequence of source refs,
             - `key`  - unique key to register watcher, same as in `clojure.core/add-watch`,
             - `f`    - function that must accept N arguments (same as number of source refs) and return a value to be written to the sink ref. Note: `f` will be called with already dereferenced values,
             - `opts` - optional. Map of:
               - `:ref` - use this as sink ref. By default creates new atom,
               - `:check-equals?` - Defaults to `true`. If equality check should be run on each source update: `(= @sink (f new-vals))`. When result of recalculating `f` equals to the old value, `reset!` won’t be called. Set to `false` if checking for equality can be expensive."}
  derived-atom derived-atom/derived-atom)


;; cursors


(defn cursor-in
  "Given atom with deep nested value and path inside it, creates an atom-like structure
   that can be used separately from main atom, but will sync changes both ways:
  
   ```
   (def db (atom { :users { \"Ivan\" { :age 30 }}}))
   
   (def ivan (rum/cursor db [:users \"Ivan\"]))
   (deref ivan) ;; => { :age 30 }
   
   (swap! ivan update :age inc) ;; => { :age 31 }
   (deref db) ;; => { :users { \"Ivan\" { :age 31 }}}
   
   (swap! db update-in [:users \"Ivan\" :age] inc)
   ;; => { :users { \"Ivan\" { :age 32 }}}
   
   (deref ivan) ;; => { :age 32 }
   ```
  
   Returned value supports `deref`, `swap!`, `reset!`, watches and metadata.
  
   The only supported option is `:meta`"
  [ref path & {:as options}]
  (if (instance? cursor/Cursor ref)
    (cursor/Cursor. (.-ref ref) (into (.-path ref) path) (:meta options))
    (cursor/Cursor. ref path (:meta options))))

(defn cursor
  "Same as [[cursor-in]] but accepts single key instead of path vector."
  [ref key & options]
  (apply cursor-in ref [key] options))

;; hooks

(defn ^array use-state
  "Takes initial value or value returning fn and returns a tuple of [value set-value!],
  where `value` is current state value and `set-value!` is a function that schedules re-render.

  (let [[value set-state!] (rum/use-state 0)]
    [:button {:on-click #(set-state! (inc value))}
      value])"
  [value-or-fn]
  (.useState js/React value-or-fn))

(defn ^array use-reducer
  "Takes reducing function and initial state value.
  Returns a tuple of [value dispatch!], where `value` is current state value and `dispatch` is a function that schedules re-render.

  (defmulti value-reducer (fn [value event] event))

  (defmethod value-reducer :inc [value _]
    (inc value))

  (let [[value dispatch!] (rum/use-reducer value-reducer 0)]
    [:button {:on-click #(dispatch! :inc)}
      value])

  Read more at https://reactjs.org/docs/hooks-reference.html#usereducer"
  ([reducer-fn initial-value]
   (.useReducer js/React #(reducer-fn %1 %2) initial-value identity)))

(defn use-effect!
  "Takes setup-fn that executes either on the first render or after every update.
  The function may return cleanup-fn to cleanup the effect, either before unmount or before every next update.
  Calling behavior is controlled by deps argument.

  (rum/use-effect!
    (fn []
      (.addEventListener js/window \"load\" handler)
      #(.removeEventListener js/window \"load\" handler))
    []) ;; empty deps collection instructs React to run setup-fn only once on initial render
        ;; and cleanup-fn only once before unmounting

  Read more at https://reactjs.org/docs/hooks-effect.html"
  ([setup-fn]
   (.useEffect js/React #(or (setup-fn) js/undefined)))
  ([setup-fn deps]
   (->> (if (array? deps) deps (into-array deps))
        (.useEffect js/React #(or (setup-fn) js/undefined)))))

(defn use-layout-effect!
  "(rum/use-layout-effect!
    (fn []
      (.addEventListener js/window \"load\" handler)
      #(.removeEventListener js/window \"load\" handler))
    []) ;; empty deps collection instructs React to run setup-fn only once on initial render
        ;; and cleanup-fn only once before unmounting

  Read more at https://reactjs.org/docs/hooks-effect.html"
  ([setup-fn]
   (.useLayoutEffect js/React #(or (setup-fn) js/undefined)))
  ([setup-fn deps]
   (->> (if (array? deps) deps (into-array deps))
        (.useLayoutEffect js/React #(or (setup-fn) js/undefined)))))

(defn use-callback
  "Takes callback function and returns memoized variant, memoization is done based on provided deps collection.

  (rum/defc button < rum/static
    [{:keys [on-click]} text]
    [:button {:on-click on-click}
      text])

  (rum/defc app [v]
    (let [on-click (rum/use-callback #(do-stuff v) [v])]
      ;; because on-click callback is memoized here based on v argument
      ;; the callback won't be re-created on every render, unless v changes
      ;; which means that underlying `button` component won't re-render wastefully
      [button {:on-click on-click}
        \"press me\"]))

  Read more at https://reactjs.org/docs/hooks-reference.html#usecallback"
  ([callback]
   (.useCallback js/React callback))
  ([callback deps]
   (->> (if (array? deps) deps (into-array deps))
        (.useCallback js/React callback))))

(defn use-memo
  "Takes a function, memoizes it based on provided deps collection and executes immediately returning a result.
  Read more at https://reactjs.org/docs/hooks-reference.html#usememo"
  ([f]
   (.useMemo js/React f))
  ([f deps]
   (->> (if (array? deps) deps (into-array deps))
        (.useMemo js/React f))))

(defn use-ref
  "Takes a value and puts it into a mutable container which is persisted for the full lifetime of the component.
  https://reactjs.org/docs/hooks-reference.html#useref"
  ([initial-value]
   (.useRef js/React initial-value)))

;; Refs

(defn create-ref []
  (.createRef js/React))

(defn deref
  "Takes a ref returned from use-ref and returns its current value."
  [^js ref]
  (.-current ref))

(defn set-ref! [^js ref value]
  (set! (.-current ref) value))

;;; Server-side rendering

;; Roman. For Node.js runtime we require "react-dom/server" for you
;; In the browser you have to add cljsjs/react-dom-server yourself

(defn render-html
  "Main server-side rendering method. Given component, returns HTML string with static markup of that component.
  Serve that string to the browser and [[hydrate]] same Rum component over it. React will be able to reuse already existing DOM and will initialize much faster.
  No opts are supported at the moment."
  ([element]
   (render-html element nil))
  ([element opts]
   (if-not (identical? *target* "nodejs")
     (.renderToString js/ReactDOMServer element)
     (let [^js/ReactDOMServer react-dom-server (js/require "react-dom/server")]
       (.renderToString react-dom-server element)))))

(defn render-static-markup
  "Same as [[render-html]] but returned string has nothing React-specific.
  This allows Rum to be used as traditional server-side templating engine."
  [src]
  (if-not (identical? *target* "nodejs")
    (.renderToStaticMarkup js/ReactDOMServer src)
    (let [^js/ReactDOMServer react-dom-server (js/require "react-dom/server")]
      (.renderToStaticMarkup react-dom-server src))))

;; JS components adapter
(defn adapt-class-helper [type attrs children]
  (let [args (.concat #js [type attrs] children)]
    (.apply (.-createElement js/React) js/React args)))
