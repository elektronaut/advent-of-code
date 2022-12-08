(ns advent-of-code.2020.19
  (:require [clojure.string :as str]))

(defn parse-rule [s]
  (let [[key values] (str/split s #": ")
        rules (->> (str/split values #" \| ")
                   (map #(read-string (str "[" % "]"))))]
    {(Integer/parseInt key) rules}))

(defn test-message [rules r message]
  (mapcat
   (fn [group]
     (reduce
      (fn [strs rule]
        (if (string? rule)
          (map #(apply str (rest %)) (filter #(= rule (str (first %))) strs))
          (mapcat #(test-message rules rule %) strs))
        ) [message] group))
   (rules r)))

(defn matches? [rules message]
  (let [result (first (test-message rules 0 message))]
    (and (empty? result)
         (string? result))))

(let [blocks (map str/split-lines (str/split (slurp "2020/day19/input.txt") #"\n\n"))
      rules (into {} (map parse-rule (first blocks)))
      messages (last blocks)
      new-rules {8 '([42] [42 8]) 11 '([42 31] [42 11 31])}]
  (println "Part 1:"
           (count (filter #(matches? rules %) messages)))
  (println "Part 2:"
           (count (filter #(matches? (merge rules new-rules) %) messages))))
