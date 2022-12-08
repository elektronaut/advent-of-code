(ns advent-of-code.2020.09
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def numbers
  (vec (->> (slurp "2020/day09/input.txt")
            (str/split-lines)
            (map #(Long/parseLong %)))))

(defn find2 [numbers n]
  (filter #(some #{%} numbers)
          (map #(- n %) numbers)))

(defn valid? [n]
  (let [nums (subvec numbers (- n 25) n)]
    (>= (count (find2 nums (nth numbers n))) 2)))

(defn invalid [numbers]
  (->> (range 25 (count numbers))
       (filter #(not (valid? %)))
       (map #(nth numbers %))))

(defn filter-max [max nums]
  (vec (->> (reduce #(if (<= (reduce + %1) max) (cons %2 %1) %1) [] nums)
            (rest)
            (reverse))))

(def first-result
  (first (invalid numbers)))

(def second-result
  (let [cnt (count numbers)]
    (first (->> (range 0 (dec cnt))
                (map #(subvec numbers % cnt))
                (map #(filter-max first-result %))
                (filter #(> (count %) 2))
                (filter #(= first-result (reduce + %)))))))

(println "Part 1:" first-result)
(println "Part 2:"
         (+ (reduce min second-result)
            (reduce max second-result)))
