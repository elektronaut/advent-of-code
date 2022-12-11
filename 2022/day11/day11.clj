(ns advent-of-code.2022.10
  (:require [clojure.string :as str]))

(defn make-operation [input]
  (let [[a operand b] (str/split input #" ")]
    (->> (str/join ["(fn [old] (" operand " " a " " b "))"])
         (read-string)
         (eval))))

(defn parse-monkey [input]
  (let [lines (str/split-lines input)
        items-str (last (str/split (lines 1) #": "))
        operation-str (last (str/split (lines 2) #"= "))]
    {:items (read-string (str/join ["[" items-str "]"]))
     :inspect-count 0
     :operation (make-operation operation-str)
     :test (read-string (last (str/split (lines 3) #"by " )))
     :if-true (read-string (last (str/split (lines 4) #"monkey ")))
     :if-false (read-string (last (str/split (lines 5) #"monkey ")))}))

(def monkeys
  (->> (str/split (slurp "2022/day11/input.txt") #"\n\n")
       (map parse-monkey)))
