(ns the-joy-of-clojure.core-test
  (:use clojure.test the-joy-of-clojure.core [clojure.set :only [intersection union difference]]))

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
  (let [s #{:a :b :c :d}
        s1 #{:humans :fruits :zombies}
        s2 #{:chupaa :humans :zombies}]
    (is (= (s :a) :a))
    (is (= (s :e) nil))
    (is (= (intersection s1 s2) #{:humans :zombies}))
    (is (= (union s1 s2) #{:humans :fruits :zombies :chupaa}))
    (is (= (difference s1 s2) #{:fruits}))
    (is (= (difference s2 s1) #{:chupaa}))))

(deftest test-hash-map-operations
  (let [m1 (hash-map :a 1 :b 2 :c 3)
        m2 (into {} [[:a 1] [:b 2]])
        m3 (into {} '([:a 1] [:b 2]))
        m4 (into {} (map vec '[(:a 1) (:b 2)]))
        m5 (zipmap [:a :b] [1 2])]
    (is (= m1 {:a 1 :b 2 :c 3}))
    (is (= m2 {:a 1 :b 2}))
    (is (= m3 {:a 1 :b 2}))
    (is (= m4 {:a 1 :b 2}))
    (is (= m5 {:a 1 :b 2}))))

(deftest test-sorted-map
  (let [m1 (sorted-map "abc" 1 "bca" 2)
        m2 (sorted-map-by #(compare (subs %1 1) (subs %2 1)) "abc" 1 "bac" 2)
        m3 (assoc (hash-map 1 :int) 1.0 :float)
        m4 (assoc (sorted-map 1 :int) 1.0 :float)
        m5 (array-map :a 1 :b 2 :c 3)]
    (println m1)
    (is (= m1 {"abc" 1 "bca" 2}))
    (is (= m2 {"bac" 2 "abc" 1}))
    (is (= m3 {1 :int 1.0 :float}))
    (is (= m4 {1 :float}))
    (is (= (seq m5) '([:a 1] [:b 2] [:c 3])))))

(deftest test-pos
  (let [m1 (hash-map :a 1 :b 2 :c 3)
        v1 [:a 1 :b 2 :c 3]]
    (is (= (pos 3 m1) :c))
    (is (= (pos 2 v1) 3))
    (is (= nil (pos 3 {})))
    (is (= nil (pos 3 {:a 1 :b 2 :c 4})))
    (is (= nil (pos 3 [1 2 4 6])))))

(deftest test-index
  (let [m1 (sorted-map :a 1 :b 2 :c 3)
        v1 [:a 1 :b 2]
        s1 #{:a :b}]
    (is (= (index m1) '([:a 1] [:b 2] [:c 3])))
    (is (= (index v1) '([0 :a] [1 1] [2 :b] [3 2])))
    (is (= (index s1) '([:a :a] [:b :b])))))

(deftest test-ppos
  (let [m1 (sorted-map :a 1 :b 2 :c 3)]
    (println (ppos #{2 3} m1))
    (is (= (ppos #{2 3} m1) '(:b :c)))))

(deftest test-persistentness
  (let [base '(:barnabas :adam)
        ls1 (cons :willie base)
        ls2 (cons :pheonix base)]
    (is (identical? (next ls1) (next ls2)))))

(deftest test-xconj
  (let [t (xconj nil 5)
        t1 (xconj t 4)
        t2 (xconj t1 6)]
    (is (= (:val t) 5))
    (is (= (:L t) nil))
    (is (= (:R t) nil))
    (is (= (:val (:L t1)) 4))
    (is (identical? (:val (:L t1)) (:val (:L t2))))))

(deftest test-and-chain
  (let [f #(+ %1 %2 %3)
        r (and-chain 1 2 3 f 1 2 3)]
    (is (= r 6))))

(deftest test-rest-next-chain
  (let [lr (rest-chain 1)
        ln (next-chain 1)]
    (println (first lr))
    (println (first ln))))

(deftest test-lz-rec-steps
  (let [lz-s (lz-rec-steps [1 2 3 4])]
    (println (class lz-s))
    (is (= lz-s '(1 (2 (3 (4 ()))))))))
