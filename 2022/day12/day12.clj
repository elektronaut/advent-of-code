(ns advent-of-code.2022.12
  (:require [clojure.string :as str]))

(def inf Double/POSITIVE_INFINITY)

(defn elevation [s]
  (let [char (first s)]
    (case char
      \S 1
      \E 26
      (- (int char) 96))))

(defn coordinates [grid]
  (for [x (range 0 (count (nth grid 0)))
        y (range 0 (count grid))]
    [y x]))

(defn find-pos [grid s]
  (->> (coordinates grid)
       (filter #(= s (get-in grid %)))))

(defn in-bounds [grid [y x]]
  (and (>= y 0)
       (>= x 0)
       (< y (count grid))
       (< x (count (nth grid 0)))))

(defn neighbours [[y x]]
  [[(dec y) x] [(inc y) x]
   [y (dec x)] [y (inc x)]])

(defn reachable [grid a b]
  (<= (->> [a b]
       (map (partial get-in grid))
       (map elevation)
       (reduce -)) 1))

(defn connected [grid pos]
  (->> (neighbours pos)
       (filter (partial in-bounds grid))
       (filter (partial reachable grid pos))
       (reduce #(assoc %1 %2 1) {})))

(defn update-costs
  [g costs unvisited curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
      (fn [c nbr nbr-cost]
        (if (unvisited nbr)
          (update-in c [nbr] min (+ curr-cost nbr-cost))
          c))
      costs
      (get g curr))))

(defn dijkstra
  ([g src]
    (dijkstra g src nil))
  ([g src dst]
    (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
           curr src
           unvisited (disj (apply hash-set (keys g)) src)]
      (cond
       (= curr dst)
       (select-keys costs [dst])

       (or (empty? unvisited) (= inf (get costs curr)))
       costs
       :else
       (let [next-costs (update-costs g costs unvisited curr)
             next-node (apply min-key next-costs unvisited)]
         (recur next-costs next-node (disj unvisited next-node)))))))

(defn read-input [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map #(str/split % #""))
       (vec)))

(let [heightmap (read-input "2022/day12/input.txt")
      graph (->> (coordinates heightmap)
                 (reduce #(assoc %1 %2 (connected heightmap %2)) {}))
      start (first (find-pos heightmap "E"))
      end (first (find-pos heightmap "S"))
      distances (dijkstra graph start)]
  (println "Part 1:" (distances end))
  (println "Part 2:" (->> (find-pos heightmap "a")
                          (map distances)
                          (reduce min))))
