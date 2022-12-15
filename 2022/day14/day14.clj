(ns advent-of-code.2022.14
  (:require [clojure.string :as str]))

(def symbols
  { nil "." :sand "o" :rock "#"})

(defn parse-path [line]
  (->> (str/split line #" -> ")
       (map #(map read-string (str/split % #",")))))

(defn read-paths [filename]
  (map parse-path (str/split-lines (slurp filename))))

(defn line [[x1 y1] [x2 y2]]
  (let [[min-x max-x] (sort [x1 x2])
        [min-y max-y] (sort [y1 y2])]
    (vec (for [x (range min-x (inc max-x))
               y (range min-y (inc max-y))]
           [x y]))))

(defn lines [coords pos]
  (if (empty? coords)
    [pos]
    (concat coords (line (last coords) pos) [pos])))

(defn translate-x [grid x]
  (+ (- x 500) (quot (count (first grid)) 2)))

(defn draw [grid [x y] sym]
  (assoc-in grid [y (translate-x grid x)] sym))

(defn draw-path [grid path]
  (->> (reduce lines [] path)
       (reduce #(draw %1 %2 :rock) grid)))

(defn initial-grid [filename]
  (let [paths (read-paths filename)
        xs (mapcat #(map first %) paths)
        ys (mapcat #(map last %) paths)
        min-x (reduce min xs)
        max-x (reduce max xs)
        width (+ 3 (* 2 (reduce max [(- max-x 500) (- 500 min-x)])))
        height (inc (reduce max ys))
        grid (vec (repeat height (vec (repeat width nil))))]
    (reduce draw-path grid paths)))

(defn print-grid [grid]
  (->> (map (fn [line] (str/join (map #(symbols %) line))) grid)
       (map println)))

(do (println)
    (print-grid (initial-grid "2022/day14/example.txt")))

(do (println)
    (print-grid (initial-grid "2022/day14/input.txt")))
