(ns ^:no-doc rum.util)

(defn collect
  [key mixins]
  (let [result (transient [])]
    (doseq [m mixins]
      (when-let [elm (get m key)]
        (conj! result elm)))
    (persistent! result)))

(defn akeep
  [afn coll]
  (let [result (transient [])]
    (doseq [elm coll]
      (when-let [nelm (afn elm)]
        (conj! result nelm)))
    (persistent! result)))

(defn collect* [keys mixins]
  (into []
        (mapcat (fn [m]
                  (akeep
                   (fn [k] (get m k))
                   keys)))
        mixins))

(defn call-all-2
  [state fns]
  (reduce
   (fn [state afn] (afn state))
   state
   fns))

(defn call-all-3
  [state fns arg1]
  (reduce
   (fn [state afn] (afn state arg1))
   state
   fns))

(defn call-all-4
  [state fns arg1 arg2]
  (reduce
   (fn [state afn] (afn state arg1 arg2))
   state
   fns))

(defn into-all
  "Like `into` but supports multiple \"from\"s.
  ~3-5x faster than concat and return vector, not lazy-seq"
  ([to from       ] (into to from))
  ([to from & more]
   (persistent!
    (reduce (fn [acc in] (reduce conj! acc in))
            (transient to)
            (cons from more)))))
