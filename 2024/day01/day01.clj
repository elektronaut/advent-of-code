(ns advent-of-code.2024.01
  (:require [clojure.string :as str]))

(defn int [s]
  (Integer/parseInt s))

(defn distance [v]
  (abs (reduce - v)))

(defn similarity [list n]
  (* n (count (filter #(= % n) list))))

(defn read-input [filename]
  (->> (slurp filename)
       (str/split-lines )
       (map #(map int (str/split % #"\s+")))))

(let [lists (read-input "2024/day01/input.txt")
      list1 (sort (map first lists))
      list2 (sort (map last lists))
      distances (map distance (map vector list1 list2))
      similarities (map (partial similarity list2) list1)]
  (println "Part 1:" (reduce + distances))
  (println "Part 2:" (reduce + similarities)))
