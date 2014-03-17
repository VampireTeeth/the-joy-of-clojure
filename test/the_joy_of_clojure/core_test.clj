(ns the-joy-of-clojure.core-test
  (:use clojure.test the-joy-of-clojure.core))

(deftest test-do-blowfish
  (is (= (do-blowfish :blowfish) "not sure what to do"))
  (is (= (do-blowfish ::blowfish) "unknown argument")))

(deftest test-change-array
  (let [input (into-array [:willie :barnabas :adam])
        output (change-array input 1 :quettin)]
    (is (= input output))))

(deftest test-change-vec
  (let [ds1 [:willie :barnabas :adam]
        ds2 (change-vec ds1 :barnabas :quentin)]
    (is (not (= ds1 ds2)))
    (is (not (identical? ds1 ds2)))))

(deftest test-sequential-equality
  (is (= [1 2 3] '(1 2 3)))
  (is (not (= [1 2 3] #{1 2 3}))))

(deftest test-vec-creation
  (let [my-vec [1 2 3]
        another-my-vec (into my-vec [:a :b :c])]
    (is (= my-vec [1 2 3]))
    (is (= another-my-vec [1 2 3 :a :b :c]))))

(deftest test-vec-of
  (let [int-vec (into (vector-of :int) [Math/PI 2 1.3])
        char-vec (into (vector-of :char) [100 101 102])]
    (is (= int-vec [3 2 1]))
    (is (= char-vec [\d \e \f]))))

(deftest test-vec-operations
  (let [a-to-j (vec (map char (range 65 75)))
        matrix [[1 2 3] [4 5 6] [7 8 9]]]
    (println a-to-j)
    (println (vec (rseq a-to-j)))
    (println (vec (assoc a-to-j 4 100)))
    (println (replace {\A 12 \B 13} a-to-j))
    (println matrix)
    (println (get-in matrix [1 2]))
    (println (assoc-in matrix [1 2] :a))
    (println (update-in matrix [1 2] * 100))))

(deftest test-neighbors
  (let [size 3 yx [1 1]
        n (neighbors size yx)]
    (println n)
    (is (= n [[0 1] [2 1] [1 0] [1 2]]))))

(deftest test-stack
  (let [s [1 2 3]]
    ;;peek
    (is (= (peek s) 3))
    (println s)
    ;;pop
    (is (= (pop s) [1 2]))
    ;;push
    (is (= (conj s 6) [1 2 3 6]))))

(deftest test-strict-map
  (let [s (vec (range 5))
        r (strict-map - s)]
    (is (= r [0 -1 -2 -3 -4]))))

(deftest test-list-creation
  (is (= (conj '(2 3) 1) '(1 2 3)))
  (is (= (cons 1 '(2 3)) '(1 2 3))))

(deftest test-queue-creation
  (let [q (conj clojure.lang.PersistentQueue/EMPTY :wake-up :shower :brush-teeth)]
    (is (= (peek q) :wake-up))
    (is (= (pop q) [:shower :brush-teeth]))
    (is (= (vec (conj q :go-out)) [:wake-up :shower :brush-teeth :go-out]))))

(deftest test-set-operations
  (let [s #{:a :b :c :d}]
    (is (= (s :a) :a))
    (is (= (s :e) nil))))
