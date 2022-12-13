(ns advent-of-code.2022.13
  (:require [clojure.string :as str]))

(def pairs
  (->> (str/split (slurp "2022/day13/input.txt") #"\n\n")
       (map #(mapv read-string (str/split-lines %)))))

(def divider-packets
  [[[2]] [[6]]])

(defn packet-order [l r]
  (cond
    (and (int? l) (int? r)) (- l r)
    (and (vector? l) (int? r)) (recur l [r])
    (and (int? l) (vector? r)) (recur [l] r)
    :else (loop [[first-l & rest-l] l
                 [first-r & rest-r] r]
            (if (not first-l)
              (if (not first-r) 0 -1)
              (if (not first-r)
                1
                (let [res (packet-order first-l first-r)]
                  (if (not= 0 res) res (recur rest-l rest-r))))))))

(defn right-order [[left right]]
  (< (packet-order left right) 1))

(defn sort-packets [pairs rest]
  (->> (concat (map first pairs)
               (map last pairs)
               rest)
       (sort packet-order)))

(println "Part 1:"
         (->> pairs
              (map-indexed (fn [i pair] [i (right-order pair)]))
              (filter #(last %))
              (map first)
              (map inc)
              (reduce +)))

(println "Part 2:"
         (let [sorted-packets (sort-packets pairs divider-packets)]
           (->> divider-packets
                (map #(.indexOf sorted-packets %))
                (map inc)
                (reduce *))))
