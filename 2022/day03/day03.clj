(ns advent-of-code.2022.03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def bags
  (->> (str/split-lines (slurp "2022/day03/input.txt"))
       (map #(str/split % #""))))

(defn compartments [bag]
  (split-at (/ (count bag) 2) bag))

(defn shared-item [collections]
  (->> (map set collections)
       (reduce set/intersection)
       (first)))

(defn priority [i]
  (let [offset (if (= i (str/upper-case i)) 38 96)]
    (- (int (first i)) offset)))

(defn solve [collections]
  (->> (map shared-item collections)
       (map priority)
       (reduce +)))

(println "Part 1:" (solve (map compartments bags)))
(println "Part 1:" (solve (partition 3 bags)))
