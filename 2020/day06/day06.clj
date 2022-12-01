(require '[clojure.string :as str]
         '[clojure.set :as set])

(def groups (str/split (slurp "input.txt") #"\n\n"))

(defn questions-any [g]
  (set (str/replace g #"[^\w]" "")))

(defn questions-all [g]
  (let [member-count (count (str/split-lines g))]
    (->> (frequencies (str/replace g #"[^\w]" ""))
         (filter #(= member-count (last %)))
         (map first))))

(defn group-count [fn]
  (->> (map fn groups)
       (map count)
       (reduce +)))

(println "Part 1:" (group-count questions-any))
(println "Part 2:" (group-count questions-all))
