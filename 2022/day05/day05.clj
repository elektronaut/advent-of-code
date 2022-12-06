(require '[clojure.string :as str])

(def input (slurp "input.txt"))

(defn parse-move [move-str]
  (->> (str/split (str/replace move-str #"move " "") #"[^\d]+")
       (map #(Integer/parseInt %))))

(defn parse-stack-row [row-str]
  (let [len (/ (inc (count row-str)) 4)]
    (->> (map #(inc (* % 4)) (range 0 len))
         (map #(nth row-str %)))))

(def moves
  (->> (last (str/split input #"\n\n"))
       (str/split-lines)
       (map parse-move)))

(def initial-state
  (->> (first (str/split input #"\n\n"))
       (str/split-lines)
       (drop-last)
       (map parse-stack-row)
       (apply map vector)
       (map (fn [stack] (filter #(not= \space %) stack)))
       (vec)))

(defn step [pickup-fn stacks [amount from to]]
  (let* [crates (pickup-fn (take amount (stacks (dec from))))
         from-stack (drop amount (stacks (dec from)))
         to-stack (concat crates (stacks (dec to)))]
    (-> stacks
        (assoc (dec from) from-stack)
        (assoc (dec to) to-stack))))

(defn solve [pickup-fn]
  (->> (reduce (partial step pickup-fn) initial-state moves)
       (map first)
       (str/join)))

(println "Part 1:" (solve reverse))
(println "Part 2:" (solve identity))
