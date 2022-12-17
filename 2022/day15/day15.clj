(ns advent-of-code.2022.15
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defn parse-line [line]
  (let [[sx sy bx by] (map read-string (re-seq #"-?\d+" line))]
    {[sx sy] [bx by]}))

(def devices
  (->> (str/split-lines (slurp "2022/day15/input.txt"))
       (map parse-line)
       (reduce into {})))

(defn covered-row [y sensor]
  (let [beacon (devices sensor)
        radius (manhattan-distance sensor beacon)
        distance (abs (- y (last sensor)))
        offset (- radius distance)
        start (- (first sensor) offset)]
    (if (> distance radius)
      '()
      (range start (+ start (inc (* 2 offset)))))))

(defn count-covered [y]
  (let [covered (into #{} (mapcat (partial covered-row y) (keys devices)))
        sensors (into #{} (filter #(= y (last %)) (vals devices)))]
    (- (count covered) (count sensors))))

(println "Part 1:" (count-covered 2000000))
