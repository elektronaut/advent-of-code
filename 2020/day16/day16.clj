(ns advent-of-code.2020.16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-long [s]
  (Long/parseLong s))

(defn parse-rule [str]
  (let [[key values] (str/split str #": ")]
    [key (->> (str/split values #" or ")
             (map #(map parse-long (str/split % #"\-"))))]))

(defn parse-tickets [str]
  (->> (str/split-lines str)
       (rest)
       (map #(->> (str/split % #",")))
       (map #(map parse-long %))))

(def data
  (let [input (str/split (slurp "2020/day16/input.txt") #"\n\n")]
    {:rules (map parse-rule (str/split-lines (input 0)))
     :ticket (first (parse-tickets (input 1)))
     :tickets (parse-tickets (input 2))}))

(defn in-range? [[min max] n]
  (and (>= n min) (<= n max)))

(defn match-rule? [rule n]
  (let [ranges (second rule)]
    (> (count (filter #(in-range? % n) ranges)) 0)))

(defn find-rules [n]
  (filter #(match-rule? % n) (data :rules)))

(defn error-rate [data]
  (->> (flatten (data :tickets))
       (filter #(= (count (find-rules %)) 0))
       (reduce +)))

(defn valid? [ticket]
  (= 0 (->> (map find-rules ticket)
            (map count)
            (filter #(= % 0))
            (count))))

(defn find-rules-all [ns]
  (->> (map find-rules ns)
       (map #(set %))
       (reduce set/intersection (set (data :rules)))))

(defn resolve-once [columns]
  (let [resolved (map first (filter #(= 1 (count %)) columns))]
    (letfn [(filter-resolved [n] (= (.indexOf resolved n) -1))
            (resolve [c] (if (> (count c) 1) (filter filter-resolved c) c))]
      (map resolve columns))))

(defn resolve-rules [columns]
  (loop [cols (resolve-once columns)]
    (if (= (reduce max (map count cols)) 1)
      (map first cols)
      (recur (resolve-once cols)))))

(defn column-indices [rules str]
  (->> (map-indexed (fn [i r] [i r]) rules)
       (filter (fn [[i r]] (str/starts-with? (first r) "departure")))
       (map first)))

(println "Part 1:" (error-rate data))

(let [valid (filter valid? (data :tickets))
      columns (->> (map-indexed (fn [i _] (map #(nth % i) valid)) (first valid))
                   (map find-rules-all)
                   (resolve-rules))
      indices (column-indices columns "departure")
      values (map #(nth (data :ticket) %) indices)]
  (println "Part 2:" (reduce * values)))
