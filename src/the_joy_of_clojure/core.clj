(ns the-joy-of-clojure.core
  (:gen-class))

(defn change-vec [vec old-val new-val]
  (replace {old-val new-val} vec))

(defn change-array [ds idx val]
  (aset ds idx val)
  ds)

(defn do-blowfish [directive]
  (case directive
    :the-joy-of-clojure.aquarium/blowfish "feed the fish"
    :the-joy-of-clojure.crypto/blowfish "encode the message"
    :blowfish "not sure what to do"
    "unknown argument"))

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
     (vec
      (filter (fn [new-yx] (every? #(< -1 % size) new-yx))
       (map #(vec (map + yx %)) deltas)))))

(defn strict-map [f coll]
  (loop [coll coll res []]
    (if (empty? coll)
      res
      (recur (next coll) (conj res (f (first coll)))))))

(defn pos [e coll]
  (let [cmp (if (map? coll)
                #(= (second %1) %2)
                #(= %1 %2))]
    (loop [p 0 s (seq coll)]
      (if (empty? s)
        nil
        (if (cmp (first s) e)
          (if (map? coll) (first (first s)) p)
          (recur (inc p) (next s)))))))

(defn index [coll]
  (cond
   (map? coll) (seq coll)
   (set? coll) (map vector coll coll)
   :else (map vector (iterate inc 0) coll)))

(defn ppos [pred coll]
  (for [[i v] (index coll) :when (pred v)] i))

(defn xconj [t v]
  (cond
   (nil? t) {:val v :L nil :R nil}
   (< v (:val t)) (assoc t :L (xconj (:L t) v))
   :else (assoc t :R (xconj (:R t) v))))

(defn and-chain [x y z func & args]
  (and x y z (apply func args)))

(defn rest-chain [v]
  (-> (iterate #(do (print \.) (inc %)) v) rest rest rest))

(defn next-chain [v]
  (-> (iterate #(do (print \.) (inc %)) v) next next next))

(defn lz-rec-steps [s]
  (lazy-seq
   (if (seq s)
     [(first s) (lz-rec-steps (rest s))]
     [])))


(defn -main [& args]
  (println "Hello, world!"))
