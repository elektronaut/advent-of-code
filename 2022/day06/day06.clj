(ns advent-of-code.2022.06
  (:require [clojure.string :as str]))

(def input (slurp "2022/day06/input.txt"))

(defn is-marker [length pos]
  (let [str (subs input (- pos length) pos)]
    (= (count str) (count (set str)))))

(defn markers [length]
  (->> (range length (inc (count input)))
       (filter (partial is-marker length))))

(println "Part 1:" (first (markers 4)))
(println "Part 1:" (first (markers 14)))
