(ns advent-of-code.2022.08
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def trees
  (->> (slurp "2022/day08/input.txt")
       (str/split-lines)
       (map (fn [line] (vec (map #(Integer/parseInt %) (str/split line #"")))))
       (vec)))

(defn sight-lines [[x y]]
  (let* [row (nth trees y)
         column (vec (map #(nth % x) trees))]
    [(subvec column 0 y) (subvec row 0 x)
     (subvec column (inc y)) (subvec row (inc x))]))

(defn visible [[x y]]
  (let* [value (get-in trees [y x])]
    (->> (sight-lines [x y])
         (map #(filter (partial <= value) %))
         (filter empty?)
         (seq))))

(defn coordinates [grid]
  (for [x (range 0 (count (nth grid 0)))
        y (range 0 (count grid))]
    [x y]))

(println "Part 1:" (->> (coordinates trees)
                        (filter visible)
                        (count)))
