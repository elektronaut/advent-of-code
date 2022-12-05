(require '[clojure.string :as str])

(defn int [s]
  (Integer/parseInt s))

(defn parse-move [move-str]
  (map int (str/split (str/replace move-str #"move " "") #"[^\d]+")))

(defn parse-stack-row [row-str]
  (let [len (/ (inc (count row-str)) 4)]
    (->> (map #(inc (* % 4)) (range 0 len))
         (map #(nth row-str %)))))

(def moves
  (->> (last (str/split (slurp "input.txt") #"\n\n"))
       (str/split-lines)
       (map parse-move)))

(def initial-state
  (->> (first (str/split (slurp "input.txt") #"\n\n"))
       (str/split-lines)
       (drop-last)
       (map parse-stack-row)
       (apply map vector)
       (map (fn [stack] (filter #(not= \space %) stack)))))
