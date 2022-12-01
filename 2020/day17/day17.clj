(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn surrounding-space [[head & tail]]
  (let [heads (map #(+ head %) '(-1 0 1))]
    (if (empty? tail)
      (map list heads)
      (mapcat (fn [t] (map #(cons % t) heads)) (surrounding-space tail)))))

(defn neighbours [c]
  (set (filter #(not= c %) (surrounding-space c))))

(defn search-space [cs]
  (set (reduce set/union (map surrounding-space cs))))

(defn rule [cs c]
  (let [n-count (count (set/intersection cs (neighbours c)))]
    (if (contains? cs c)
      (or (= n-count 2) (= n-count 3))
      (= n-count 3))))

(defn next-state [cs]
  (set (filter #(rule cs %) (search-space cs))))

(defn steps [cs]
  (lazy-seq (cons cs (steps (next-state cs)))))

(defn parse-input [str]
  (set (->> (let [lines (str/split-lines str)]
              (for [y (range 0 (count lines))
                    x (range 0 (count (first lines)))]
                (list (nth (nth lines y) x) x y)))
            (filter #(= \# (first %)))
            (map rest))))

(defn stretch-dimensions [cs dimensions]
  (set (map #(concat % (repeat (- dimensions 2) 0)) cs)))

(defn boot-count [cs]
  (count (nth (steps cs) (inc 6))))

(let [initial-state (parse-input (slurp "input.txt"))]
  (println "Part 1:" (boot-count (stretch-dimensions initial-state 3)))
  (println "Part 2:" (boot-count (stretch-dimensions initial-state 4))))
