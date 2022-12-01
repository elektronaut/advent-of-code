(require '[clojure.string :as str])

(def lines (str/split-lines (slurp "input.txt")))

(defn count-trees [v]
  (count (filter #(= \# %) v)))

(defn read-pos [[x y]]
  (let [line (get lines y)]
    (get line (mod x (count line)))))

(defn path-seq
  ([path] (path-seq path 0))
  ([path index] (lazy-seq (cons (map #(* index %) path)
                                (path-seq path (inc index))))))

(defn trace-path [path]
  (map read-pos
       (take-while #(< (last %) (count lines))
                   (path-seq path))))

(println "Part 1:"
         (count-trees (trace-path [3 1])))

(println "Part 2:"
         (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
              (map trace-path)
              (map count-trees)
              (reduce *)))
