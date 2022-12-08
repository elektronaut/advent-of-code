(ns advent-of-code.2020.05
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def passes
  (str/split-lines (slurp "2020/day05/input.txt")))

;; The input is just a binary number where F/L = 0 and B/R = 1,
;; there's no need to split the string and calculate column/row.
(defn seat-id [s]
  (Integer/parseInt
   (str/replace (str/replace s #"[FL]" "0") #"[BR]" "1") 2))

(println "Part 1:" (reduce max (map seat-id passes)))

(let [taken-seats (map seat-id passes)
      seats (range (reduce min taken-seats)
                   (reduce max taken-seats))
      available-seats (set/difference (set seats)
                                      (set taken-seats))]
  (println "Part 2:" (first available-seats)))
