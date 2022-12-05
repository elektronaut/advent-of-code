(require '[clojure.string :as str])

(def input (slurp "input.txt"))

(defn int [s]
  (Integer/parseInt s))

(defn parse-move [move-str]
  (map int (str/split (str/replace move-str #"move " "") #"[^\d]+")))

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

(defn step [stacks [amount from to]]
  (let* [source (nth stacks (dec from))
         crates (take amount source)
         remaining (drop amount source)
         new-stack (concat (reverse crates) (nth stacks (dec to)))]
    (assoc (assoc stacks (dec from) remaining)
           (dec to) new-stack)))

(println "Part 1:" (->> (reduce step initial-state moves)
                        (map first)
                        (str/join)))
