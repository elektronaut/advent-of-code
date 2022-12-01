(require '[clojure.string :as str])

(def directions
  [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]])

(def initial-state
  (->> (str/split-lines (slurp "input.txt"))
       (map vec)))

(defn lookup [state [x y]]
  (nth (nth state y nil) x nil))

(defn direction-lookup [state [x y] [dir-x dir-y]]
  (lookup state [(+ x dir-x) (+ y dir-y)]))

(defn adjacent [state pos]
  (map #(direction-lookup state pos %) directions))

(defn look-direction [state [x y] [dir-x dir-y]]
  (let [next-pos [(+ x dir-x) (+ y dir-y)]
        value (lookup state next-pos)]
    (if value
      (lazy-seq (cons value (look-direction state next-pos [dir-x dir-y]))))))

(defn visible-direction [state pos dir]
  (first (filter #(not= \. %) (look-direction state pos dir))))

(defn visible [state pos]
  (map #(visible-direction state pos %) directions))

(defn count-occupied [state]
  (count (filter #(= \# %)(flatten state))))

(defn next-state [ruleset state]
  (map-indexed
   (fn [y line]
     (map-indexed
      (fn [x value] (ruleset state value [x y])) line)) state))

(defn stable-state
  ([ruleset] (stable-state ruleset initial-state))
  ([ruleset state] (let [next (next-state ruleset state)]
    (if (= state next)
      state
      (stable-state ruleset next)))))

(defn rules [look-fn max]
  (fn [state value [x y]]
    (let [occupied (count-occupied (look-fn state [x y]))]
      (cond
        (and (= \L value) (= occupied 0)) \#
        (and (= \# value) (>= occupied max)) \L
        :else value))))

(println "Part 1:" (count-occupied (stable-state (rules adjacent 4))))
(println "Part 2:" (count-occupied (stable-state (rules visible 5))))
