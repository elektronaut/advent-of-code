(ns advent-of-code.2024.03
  (:require [clojure.string :as str]))

(defn read-input [filename]
  (slurp filename))

(defn multiply-args [items]
  (->> (keep identity items)
       (map Integer/parseInt)
       (reduce *)))

(defn instructions [input]
  (->> (re-seq #"mul\((\d+),(\d+)\)|do(n't)?\(\)" input)
       (map #(cond
               (= "do()" (first %)) true
               (= "don't()" (first %)) false
               :else (multiply-args (rest %))))))

(defn enabled-instructions [program]
  ((reduce
    (fn [state item]
      (cond
        (boolean? item) (assoc state :enabled item)
        (state :enabled) (assoc state :list (conj (state :list) item))
        :else state))
    { :enabled true :list [] } program) :list))

(let [program (instructions (read-input "2024/day03/input.txt"))]
  (println "Part 1:" (->> (filter number? program)
                          (reduce +)))
  (println "Part 2:" (->> (enabled-instructions program)
                          (reduce +))))
