(require '[clojure.string :as str])

(def input (slurp "input.txt"))

(defn is-marker [pos]
  (let [str (subs input (- pos 4) pos)]
    (= (count str) (count (set str)))))

(println "Part 1:" (->> (range 4 (inc (count input)))
                        (filter is-marker)
                        (first)))
