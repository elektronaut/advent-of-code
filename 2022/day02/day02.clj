(ns advent-of-code.2022.02
  (:require [clojure.string :as str]))

(def plays (->> (slurp "2022/day02/input.txt")
                (str/split-lines)
                (map #(str/split % #" "))))

(def shapes {"A" 1 "B" 2 "C" 3
             "X" 1 "Y" 2 "Z" 3})

(defn outcome-points [a b]
  ([3 0 6]
   (mod (- (+ 3 (shapes a)) (shapes b)) 3)))

(defn score [[a b]]
  (+ (shapes b)
     (outcome-points a b)))

(defn opposing-shape [opponent strategy]
  (let [offset ({"X" -1 "Y" 0 "Z" 1} strategy)]
    (nth (cycle ["C" "A" "B"])
         (+ (shapes opponent) offset))))

(defn opposing-play [[a b]]
  [a (opposing-shape a b)])

(println "Part 1:" (reduce + (map score plays)))

(println "Part 1:" (->> (map opposing-play plays)
                        (map score)
                        (reduce +)))
