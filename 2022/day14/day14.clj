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
        min-x (reduce min (mapcat #(map first %) paths))
        max-x (reduce max (mapcat #(map first %) paths))
        height (inc (reduce max (mapcat #(map last %) paths)))
        paths-width (+ 3 (* 2 (reduce max [(- max-x 500) (- 500 min-x)])))
        width (reduce max [(+ 3 (* 2 height)) paths-width])
        grid (vec (repeat height (vec (repeat width nil))))]
    (reduce draw-path grid paths)))

(defn print-grid [grid]
  (->> (map (fn [line] (str/join (map #(symbols %) line))) grid)
       (map println)))

(defn free? [grid [x y]]
  (not (get-in grid [y x])))

(defn sand-pos
  ([grid] (sand-pos grid [(translate-x grid 500) 0]))
  ([grid [initial-x initial-y]]
   (loop [x initial-x y initial-y]
     (cond
       (not (free? grid [x y])) :blocked
       (> y (count grid)) :overflow
       (free? grid [x (inc y)]) (recur x (inc y))
       (free? grid [(dec x) (inc y)]) (recur (dec x) (inc y))
       (free? grid [(inc x) (inc y)]) (recur (inc x) (inc y))
       :else [x y]))))

(defn count-steps [initial-grid]
  (loop [grid initial-grid
         count 0]
    (let [pos (sand-pos grid)]
      (if (or (= pos :overflow)
              (= pos :blocked))
        count
        (recur (assoc-in grid [(last pos) (first pos)] :sand)
               (inc count))))))

(defn add-floor [grid]
  (let [width (count (first grid))]
    (conj grid
          (vec (repeat width nil))
          (vec (repeat width :rock)))))

;; (do (println)
;;     (print-grid (->> (initial-grid "2022/day14/example.txt")
;;                      (add-floor))))

(let [grid (initial-grid "2022/day14/input.txt")]
  (println "Part 1:" (count-steps grid))
  (println "Part 2:" (count-steps (add-floor grid))))
