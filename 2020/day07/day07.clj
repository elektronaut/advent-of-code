(require '[clojure.string :as str])

(def lines (map #(str/replace % #".$" "")
            (str/split-lines (slurp "input.txt"))))

(defn color [s]
  (last (re-find #"([\w\s]+) bags?" s)))

(defn color-and-count [s]
  (let [match (re-find #"([\d]+) ([\w\s]+)" s)
        n (Integer/parseInt (nth match 1))
        c (color (nth match 2))]
    {c n}))

(defn parse-line [l]
  (let [[label contents] (str/split l #" contain ")]
    {(color label)
     (if (not= "no other bags" contents)
       (->> (str/split contents #", ")
            (map color-and-count)
            (apply merge))
       {})}))

(def rules (apply merge (map parse-line lines)))

(defn reverse-lookup [col]
  [col (->> (filter #((last %) col) rules)
            (map first)
            (map reverse-lookup))])

(defn bag-count [col]
  (->> (rules col)
       (map (fn [[c num]] (+ num (* num (bag-count c)))))
       (reduce +)))

(println "Part 1:"
         (count (distinct (flatten (last (reverse-lookup "shiny gold"))))))

(println "Part 2:" (bag-count "shiny gold"))
