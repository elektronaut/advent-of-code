(require '[clojure.string :as str])

(def instructions
  (->> (str/split-lines (slurp "input.txt"))
       (map #(vec [(keyword (re-find #"^[^\d]+" %))
                   (Integer/parseInt (re-find #"[\d]+$" %))]))))

(def directions
  {:N [0 1] :E [1 0] :S [0 -1] :W [-1 0]})

(defn vec* [v x]
  (vec (map #(* x %) v)))

(defn vec+ [v1 v2]
  (vec (map-indexed #(+ %2 (v2 %1)) v1)))

(defn perpendicular [[x y]]
  [y (- 0 x)])

(defn manhattan-distance [v]
  (reduce + (map #(Math/abs %) v)))

(defn turn [d degrees]
  (let [dirs (keys directions)]
    (nth dirs (mod (+ (.indexOf dirs d)
            (/ degrees 90))
                   (count dirs)))))

(defn move [pos dir value]
  (vec+ pos (vec* (directions dir) value)))

(defn rotate [v degrees]
  (last (take (inc (mod (/ degrees 90) 4))
              (iterate perpendicular v))))

(defn part1 [state [instruction value]]
  (let [{dir :dir pos :pos} state]
    (merge state
           (case instruction
             :L {:dir (turn dir (* value -1))}
             :R {:dir (turn dir value)}
             :F {:pos (move pos dir value)}
             {:pos (move pos instruction value)}))))

(defn part2 [state [instruction value]]
  (let [{pos :pos waypoint :waypoint} state]
    (merge state
           (case instruction
             :L {:waypoint (rotate waypoint (* value -1))}
             :R {:waypoint (rotate waypoint value)}
             :F {:pos (vec+ pos (vec* waypoint value))}
             {:waypoint (move waypoint instruction value)}))))

(defn solve [rules-fn initial-state]
  (manhattan-distance
   ((reduce rules-fn initial-state instructions) :pos)))

(println "Part 1:" (solve part1 {:pos [0 0] :dir :E}))
(println "Part 2:" (solve part2 {:pos [0 0] :waypoint [10 1]}))
