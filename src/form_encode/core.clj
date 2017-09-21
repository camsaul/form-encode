(ns form-encode.core
  (:require [ring.util.codec :as codec]))

(defn- encode-flatten
  "Flatten map M into a map that will produce the right output for form encoding."
  ([m]
   (into {} (for [[k v] m]
              (encode-flatten k v))))
  ([k v]
   (cond
     (map? v)
     (into {} (for [[childk childv] v]
                (encode-flatten (format "%s[%s]" (name k) (name childk)) childv)))

     (sequential? v)
     (into {} (for [i    (range (count v))
                    :let [child (nth v i)]]
                (encode-flatten (format "%s[%d]" (name k) i) child)))

     :else
     {k v})))

(defn- form-encode
  "Form-encode object `x`, correctly handling nested maps and arrays if `x` is a map."
  [x & [encoding]]
  (codec/form-encode (if (map? x)
                       (encode-flatten m)
                       x)
                     encoding))
