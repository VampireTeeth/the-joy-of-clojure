(ns the-joy-of-clojure.ch5
  (:gen-class))

(defn strict-map [f coll]
  (loop [coll coll acc []]
    (if (empty? coll)
      acc
      (recur (next coll) (conj acc (f (first coll)))))))
