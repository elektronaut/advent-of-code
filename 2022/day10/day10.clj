(ns advent-of-code.2022.10
  (:require [clojure.string :as str]))

(defn noop [x]
  [x])

(defn addx [value x]
  [x (+ value x)])

(defn parse-instruction [s]
  (if (= s "noop")
    (partial noop)
    (partial addx (Integer/parseInt (last (str/split s #" "))))))

(defn exec [state instruction]
  (let [x (last state)]
    (into state (instruction x))))

(def cycles
  (->> (str/split-lines (slurp "2022/day10/input.txt"))
       (map parse-instruction)
       (reduce exec [1])))

(defn signal-strength [cycle]
  (* cycle (nth cycles (dec cycle))))

(defn pixel [i x]
  (if (and (>= i (dec x))
           (<= i (inc x)))
    "#"
    "."))

(println "Part 1:" (->> [20 60 100 140 180 220]
                        (map signal-strength)
                        (reduce +)))

(do (println "Part 2:")
    (->> (partition 40 cycles)
         (map #(map-indexed pixel %))
         (map str/join)
         (map println)))
