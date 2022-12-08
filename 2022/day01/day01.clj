(ns advent-of-code.2022.01
  (:require [clojure.string :as str]))

(defn int [s]
  (Integer/parseInt s))

(def elves
  (->> (str/split (slurp "2022/day01/input.txt") #"\n\n")
       (map str/split-lines)
       (map #(reduce + (map int %)))
       (sort #(compare %2 %1))))

(println "Part 1:" (first elves))
(println "Part 2:" (reduce + (take 3 elves)))
