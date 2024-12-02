(ns advent-of-code.2024.02
  (:require [clojure.string :as str]))

(defn decreasing? [items]
  (= items (reverse (sort items))))

(defn increasing? [items]
  (= items (sort items)))

(defn intervals [items]
  (->> (map vector items (rest items))
       (map #(reduce - %))
       (map abs)))

(defn safe? [items]
  (let [min (apply min (intervals items))
        max (apply max (intervals items))]
    (and (or (increasing? items)
             (decreasing? items))
         (> min 0) (< max 4))))

(defn drop-nth [items n]
  (keep-indexed #(when (not= n %1) %2) items))

(defn permutations [items]
  (concat [items]
          (map-indexed (fn [idx _] (drop-nth items idx)) items)))

(defn safe-dampened? [items]
  (some safe? (permutations items)))

(defn read-input [filename]
  (->> (slurp filename)
       (str/split-lines )
       (map #(map Integer/parseInt (str/split % #"\s+")))))

(let [reports (read-input "2024/day02/input.txt")]
  (println "Part 1:" (count (filter safe? reports)))
  (println "Part 2:" (count (filter safe-dampened? reports))))
