(require '[clojure.string :as str])

(def elves
  (->> (str/split (slurp "input.txt") #"\n\n")
       (map str/split-lines)
       (map (fn [lines] (reduce + (map #(Integer/parseInt %) lines))))
       (sort #(compare %2 %1))))

(println "Part 1:" (first elves))
(println "Part 2:" (reduce + (take 3 elves)))
