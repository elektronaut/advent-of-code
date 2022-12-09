(ns advent-of-code.2022.08
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def trees
  (->> (slurp "2022/day08/input.txt")
       (str/split-lines)
       (map (fn [line] (vec (map #(Integer/parseInt %) (str/split line #"")))))
       (vec)))

(defn coordinates [grid]
  (for [x (range 0 (count (nth grid 0)))
        y (range 0 (count grid))]
    [x y]))

(defn sight-lines [[x y]]
  (let* [row (nth trees y)
         column (vec (map #(nth % x) trees))]
    [(reverse (subvec column 0 y)) (reverse (subvec row 0 x))
     (subvec column (inc y)) (subvec row (inc x))]))

(defn visible [[x y]]
  (let* [value (get-in trees [y x])]
    (->> (sight-lines [x y])
         (map #(filter (partial <= value) %))
         (filter empty?)
         (seq))))

(defn view-distance [max coll]
  (let [visible-tree (first (filter #(>= % max) coll))]
    (if visible-tree
      (inc (.indexOf coll visible-tree))
      (count coll))))

(defn scenic-score [[x y]]
  (let [height (get-in trees [y x])]
    (->> (sight-lines [x y])
         (map (partial view-distance height))
         (reduce *))))

(println "Part 1:" (->> (coordinates trees)
                        (filter visible)
                        (count)))

(println "Part 2:" (->> (coordinates trees)
                        (map scenic-score)
                        (reduce max)))
