(ns advent-of-code.2022.11
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
       (map parse-monkey)
       (vec)))

(defn process-item [worry-fn monkey monkeys item]
  (let [worry-level (worry-fn ((monkey :operation) item))
        target (if (= 0 (mod worry-level (monkey :test)))
                 (monkey :if-true)
                 (monkey :if-false))
        target-monkey (nth monkeys target)
        target-items (conj (target-monkey :items) worry-level)]
    (assoc monkeys target (assoc target-monkey :items target-items))))

(defn process-monkey [worry-fn monkeys index]
  (let [monkey (nth monkeys index)
        items (monkey :items)
        next-monkeys (reduce (partial process-item worry-fn monkey) monkeys items)]
    (assoc next-monkeys index
           (-> monkey
               (assoc :items [])
               (assoc :inspect-count (+ (count items) (monkey :inspect-count)))))))

(defn round [worry-fn monkeys]
  (reduce (partial process-monkey worry-fn) monkeys (range 0 (count monkeys))))

(defn solve [worry-fn rounds]
  (->> (nth (iterate (partial round worry-fn) monkeys) rounds)
       (sort-by :inspect-count)
       (reverse)
       (take 2)
       (map :inspect-count)
       (reduce *)))

(println "Part 1:" (solve #(quot % 3) 20))

(println "Part 1:"
         (let [cycle-length (reduce * (map :test monkeys))]
           (solve #(mod % cycle-length) 10000)))
