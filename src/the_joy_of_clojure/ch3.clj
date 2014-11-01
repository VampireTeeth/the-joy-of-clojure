(ns the-joy-of-clojure.ch3
  (:gen-class))


(defn print-seq [s]
  (when (seq s)
    (println (first s))
    (recur (rest s))))

(defn whole-name [names]
  (let [[f-name m-name l-name] names]
    (println f-name m-name l-name)))

(defn first-3-and-rest [s]
  (let [[a b c & more :as all] s]
    (println "a b c are:" a b c)
    (println "more is:" more)
    (println "all is:" all)))

(defn destructure-name-map [name-map]
  (let [{:keys [title f-name m-name l-name], :or {title "Mr."}} name-map]
    (println title f-name m-name l-name)))
