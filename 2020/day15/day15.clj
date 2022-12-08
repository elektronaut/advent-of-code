(ns advent-of-code.2020.15)

(def starting-numbers [20 0 1 11 6 3])

(defn number-sequence
  ([init] (number-sequence init 0 nil {} {}))
  ([init turn last-num prev-log log]
   (let [init-num (nth init turn nil)
         prev (if (prev-log last-num)
                (- (dec turn) (prev-log last-num)))
         num (or init-num prev 0)
         next-log (assoc log num turn)]
     (lazy-seq
      (cons num (number-sequence init (inc turn) num log next-log))))))

(defn nth-turn [init n]
  (last (take n (number-sequence init))))

(println "Part 1:" (nth-turn starting-numbers 2020))
(println "Part 2:" (nth-turn starting-numbers 30000000))
