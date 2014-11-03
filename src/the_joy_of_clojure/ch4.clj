(ns the-joy-of-clojure.ch4
  (:gen-class))

(defn overflow-demo []
  (+ Integer/MAX_VALUE Integer/MAX_VALUE))

(defn underflow-demo []
  (float 0.000000000000000000000001))

(defn rounding-error-demo []
  (+ 0.1M 0.1M 0.1M 0.1 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M))

(defn rational-demo []
  (let [a (rationalize 1.00e50)
        b (rationalize -1.00e50)
        c (rationalize 17.00e00)]
    (+ (+ a b) c)))
