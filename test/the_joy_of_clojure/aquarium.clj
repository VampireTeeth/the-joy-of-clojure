(ns the-joy-of-clojure.aquarium (:gen-class)
    (:use clojure.test the-joy-of-clojure.core))

(deftest test-do-blowfish
  (is (= (do-blowfish ::blowfish) "feed the fish")))
