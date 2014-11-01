(ns the-joy-of-clojure.ch3-test
  (:use clojure.test the-joy-of-clojure.ch3))

(deftest test-print-seq
  (print-seq [1 2 3 4 5]))

(deftest test-whole-name
  (whole-name ["Steven" "Weike" "Liu"]))

(deftest test-3-and-rest
  (first-3-and-rest (range 10)))

(deftest test-destructure-name-map
  (destructure-name-map {:f-name "Steven" :m-name "Weike" :l-name "Liu"})
  (destructure-name-map {:title "Ms." :f-name "Ashly" :m-name "Linglong" :l-name "Li"}))
