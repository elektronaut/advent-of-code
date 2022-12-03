(require '[clojure.string :as str])

(defn shared-item [bag]
  (let [[a b] (map set bag)]
    (first (clojure.set/intersection a b))))

(def bags
  (->> (str/split-lines (slurp "input.txt"))
       (map #(str/split % #""))
       (map #(split-at (/ (count %) 2) %))))

(defn priority [i]
  (let [offset (if (= i (str/upper-case i)) 38 96)]
    (- (int (first i)) offset)))

(println "Part 1:" (->> (map shared-item bags)
                        (map priority)g
                        (reduce +)))
