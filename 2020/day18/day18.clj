(ns advent-of-code.2020.18
  (:require [clojure.string :as str]))

(defn prefix [pre items]
  (cons pre (map #(if (list? %) (prefix pre %) %) items)))

(defn calc-no-precedence [init & ops]
  (reduce (fn [v [op n]] (apply op [v n])) init (partition 2 ops)))

(defn calc-precedence [& items]
  (->> (partition-by #(= * %) items)
       (map #(filter number? %))
       (filter #(not (empty? %)))
       (map #(apply + %))
       (reduce *)))

(let [sexps (->> (str/split-lines (slurp "2020/day18/input.txt"))
                 (map #(read-string (str "(" % ")"))))]
  (println "Part 1:" (reduce + (map #(eval (prefix 'calc-no-precedence %)) sexps)))
  (println "Part 2:" (reduce + (map #(eval (prefix 'calc-precedence %)) sexps))))
