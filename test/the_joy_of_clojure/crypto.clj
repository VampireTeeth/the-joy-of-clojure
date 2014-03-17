(ns the-joy-of-clojure.crypto (:gen-class)
    (:use clojure.test the-joy-of-clojure.core))

(deftest test-do-blowfish
  (is (= (do-blowfish ::blowfish) "encode the message")))
