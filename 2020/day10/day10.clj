(require '[clojure.string :as str])

(def adapters
  (->> (slurp "input.txt")
       (str/split-lines)
       (map #(Integer/parseInt %))
       (sort)))

(def differences
  (conj (reduce #(conj %1 (- %2 (reduce + %1))) [] adapters) 3))

(println "Part 1:"
         (let [freqs (frequencies differences)]
           (* (freqs 1) (freqs 3))))

(println "Part 2:"
         (->> (partition-by identity differences)
              (filter #(some #{1} %))
              (map count)
              (map #({1 1, 2 2, 3 4, 4 7, 5 13} %))
              (reduce *)))
