(ns ^:no-doc rum.util)

(defn collect [key mixins]
  (into []
        (keep (fn [m] (get m key)))
        mixins))

(defn collect* [keys mixins]
  (into []
        (mapcat (fn [m] (keep (fn [k] (get m k)) keys)))
        mixins))

(defn call-all [state fns & args]
  (reduce
   (fn [state fn]
     (apply fn state args))
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
