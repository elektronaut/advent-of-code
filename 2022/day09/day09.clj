(ns advent-of-code.2022.09
  (:require [clojure.string :as str]))

(defn vec+ [[a1 a2] [b1 b2]]
  [(+ a1 b1) (+ a2 b2)])

(defn vec- [[a1 a2] [b1 b2]]
  [(- a1 b1) (- a2 b2)])

(defn vec-abs [v]
  (vec (map abs v)))

(defn distance [a b]
  (reduce max (vec-abs (vec- a b))))

(defn clamp [n min max]
  (if (< n min) min (if (> n max) max n)))>

(defn normalize [v]
  (vec (map #(clamp % -1 1) v)))

(defn move-tail [head tail]
  (if (> (distance head tail) 1)
    (vec+ tail (normalize (vec- head tail)))
    tail))

(defn parse-move [move]
  (let [[dir amount] (str/split move #" ")]
    (repeat (Integer/parseInt amount) dir)))

(def moves
  (->> (str/split-lines (slurp "2022/day09/input.txt"))
       (mapcat parse-move)))

(def directions
  {"U" [0 -1] "D" [0 1] "L" [-1 0] "R" [1 0]})

(defn move [history dir]
  (let [[head tail] (first history)
        next-head (vec+ head (directions dir))
        next-tail (move-tail next-head tail)]
    (conj history [next-head next-tail])))

(println "Part 1:" (->> (reduce move '([[0 0][0 0]]) moves)
                        (map last)
                        (set)
                        (count)))
