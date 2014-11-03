(ns the-joy-of-clojure.ch4-test
  (:use clojure.test the-joy-of-clojure.ch4))

(deftest test-overflow-demo
  (println "Result is" (overflow-demo)))

(deftest test-underflow-demo
  (println "Result is" (underflow-demo)))

(deftest test-rounding-error-demo
  (println "Result is" (rounding-error-demo)))

(deftest test-rational-demo
  (let [r (rational-demo)]
    (println "Result is" r)
    (is (= 17 r))))

(deftest test-keyword
  (let [m {:zombies 2700 :humans 9}]
    (is (= (:zombies m) 2700))
    (is (= (:humans m) 9))))

(deftest test-qualified-keyword
  (defn do-blowfish [directive]
    (case directive
        :aquarium/blowfish (println "feed the fish")
        :crypto/blowfish (println "encode the message")
        :blowfish (println "not sure what to do")))
  (do-blowfish :aquarium/blowfish)
  (do-blowfish :crypto/blowfish)
  (do-blowfish :blowfish))
