(ns the-joy-of-clojure.ch5-test
  (:use clojure.test the-joy-of-clojure.ch5)
  (:gen-class))

(deftest test-persitence-ds
  (let [arr1 (into-array [:aaa :bbb :ccc])
        ds1 [:babanas :eeihei :qqdd]
        ds2 (replace {:eeihei :quentin} ds1)]
    (is (= (seq arr1) (seq [:aaa :bbb :ccc])))
    (aset arr1 1 :ddd)
    (is (= (seq arr1) (seq [:aaa :ddd :ccc])))
    (is (= (seq ds1) (seq [:babanas :eeihei :qqdd])))
    (is (= (seq ds2) (seq [:babanas :quentin :qqdd])))))

(deftest test-vectors
  (is (= (seq [1 2 3 4]) (cons 1 (seq [2 3 4]))))
  (is (= (seq [1 2 3 4]) (cons 1 [2 3 4])))
  (is (= (seq [5 1 2 3 4]) (conj (seq [2 3 4]) 1 5)))
  (is (= (seq [2 3 4 1 5]) (conj [2 3 4] 1 5))))

(deftest test-strict-map
  (let [coll (range 5)]
    (is (= [-5 -4 -3 -2 -1] (strict-map #(- % 5) coll)))
    (is (= [0 -1 -2 -3 -4] (strict-map - coll)))))


(deftest test-subvector
  (let [v (vec (range 5))
        sv (subvec v 1 3)]
    (is (= [1 2] sv))))
