(ns advent-of-code.2024.04
  (:require [clojure.string :as str]))

(def puzzle
  (->> (slurp "2024/day04/input.txt")
       (str/split-lines)
       (map (fn [line] (vec (str/split line #""))))
       (vec)))

(defn coordinates [grid]
  (for [x (range 0 (count (nth grid 0)))
        y (range 0 (count grid))]
    [x y]))

(defn puzzle-line [[y x] [offset-y offset-x]]
  (lazy-seq
   (cons (get-in puzzle [y x])
         (puzzle-line [(+ y offset-y) (+ x offset-x)]
                      [offset-y offset-x]))))

(defn words-at [[y x]]
  (let [directions [[-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1]]]
    (map #(take 4 (puzzle-line [y x] %)) directions)))

(defn x-word [[y x] [offset-y offset-x]]
  (take 3 (puzzle-line [(- y offset-y) (- x offset-x)]
                       [offset-y offset-x])))

(defn x-words [[y x]]
  (let [directions [[-1 1] [1 1] [1 -1] [-1 -1]]]
    (map #(x-word [y x] %) directions)))

(defn x-mas? [words]
  (= 2 (count (filter #(= '("M" "A" "S") %) words))))

(println "Part 1:" (->> (coordinates puzzle)
                        (mapcat words-at)
                        (filter #(= '("X" "M" "A" "S") %))
                        (count)))

(println "Part 2:" (->> (coordinates puzzle)
                        (map x-words)
                        (filter x-mas?)
                        (count)))
