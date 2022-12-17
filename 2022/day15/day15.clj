(ns advent-of-code.2022.15
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defn valid-position? [[x y]]
  (and (>= x 0) (<= x 4000000)
       (>= y 0) (<= y 4000000)))

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

(defn covered?
  ([pos] (reduce #(or %1 %2) (map (partial covered? pos) (keys devices))))
  ([pos sensor] (let [beacon (devices sensor)
                      radius (manhattan-distance sensor beacon)
                      distance (manhattan-distance sensor pos)]
                  (<= distance radius))))

(defn perimeter [sensor]
  (let [[sensor-x sensor-y] sensor
        radius (inc (manhattan-distance sensor (devices sensor)))]
    (->> (for [y (range (- sensor-y radius) (inc (+ sensor-y radius)))]
           (let [distance (abs (- y sensor-y))
                 offset (- radius distance)]
             (if (= 0 offset)
               [[sensor-x y]]
               [[(- sensor-x offset) y]
                [(+ sensor-x offset) y]])))
         (mapcat identity))))

(defn tuning-freq [[x y]]
  (+ (* x 4000000) y))

(println "Part 1:" (count-covered 2000000))
(println "Part 2:"
         (->> (mapcat perimeter (keys devices))
              (filter valid-position?)
              (remove covered?)
              (first)
              (tuning-freq))
