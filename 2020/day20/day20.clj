(require '[clojure.string :as str]
         '[clojure.set :as set])

(def tiles
  (->> (str/split (slurp "input.txt") #"\n\n")
       (map str/split-lines)
       (map (fn [[head & tail]]
              {(Integer/parseInt (re-find #"[\d]+" head))
               (map #(str/split % #"") tail)}))
       (into {})))

(defn flip [tile]
  (map reverse tile))

(defn rotate [tile]
  (for [x (range 0 (count tile))]
    (vec (reverse (map #(nth % x) tile)))))

(defn rotations [tile]
  (lazy-seq (cons tile (rotations (rotate tile)))))

(defn all-permutations [tile]
  (mapcat #(take 4 (rotations %)) [tile (flip tile)]))

(defn edges [tile]
  (map first (all-permutations tile)))

(defn shared-edge? [t1 t2]
  (> (count (set/intersection (set (edges t1))
                              (set (edges t2)))) 0))

(defn neighbours [id]
  (let [others (dissoc tiles id)]
    (filter (fn [[k v]] (shared-edge? (tiles id) v))
            others)))

(defn corner-tile? [[id _]]
  (= 2 (count (neighbours id))))

(println "Part 1:"
         (->> (filter corner-tile? tiles)
              (map first)
              (reduce *)))
