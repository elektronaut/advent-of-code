(require '[clojure.string :as str])

(def inputs (str/split-lines (slurp "input.txt")))

(defn valid-pass? [s]
  (let [[min-str max-str letter pass] (str/split s #"[^\w\d]+")
        [min max] (map #(Integer/parseInt %) [min-str max-str])
        count (get (frequencies pass) (first letter) 0)]
    (and (>= count min)
         (<= count max))))

(defn valid-pass2? [s]
  (let [[pos1 pos2 letter pass] (str/split s #"[^\w\d]+")
        indices (map #(- (Integer/parseInt %) 1) [pos1 pos2])
        letters (map #(get pass %) indices)]
    (and (not= (first letters) (last letters))
         (some #{(first letter)} letters))))

(println "Part 1:" (count (filter valid-pass? inputs)))
(println "Part 2:" (count (filter valid-pass2? inputs)))
