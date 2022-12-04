(require '[clojure.string :as str])

(defn int [s]
  (Integer/parseInt s))

(defn parse-pair [line]
  (->> (str/split line #",")
       (map #(map int (str/split % #"-")))))

(def pairs
  (->> (slurp "input.txt")
       (str/split-lines)
       (map parse-pair)))

(defn cover? [a b]
  (and (>= (first b) (first a))
       (<= (last b) (last a))))

(defn either-cover? [[a b]]
  (or (cover? a b) (cover? b a)))

(defn overlaps? [[a b]]
  (and (>= (last b) (first a))
       (<= (first b) (last a))))

(println "Part 1:" (count (filter either-cover? pairs)))
(println "Part 1:" (count (filter overlaps? pairs)))
