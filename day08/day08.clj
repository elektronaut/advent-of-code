(require '[clojure.string :as str])

(def initial-state {:pc 0 :accumulator 0 :log []})

(def program
  (->> (str/split-lines (slurp "input.txt"))
       (map #(let [[o a] (str/split % #"\s")] [o (Integer/parseInt a)]))
       (vec)))

(defn step [instructions state]
  (let [{pc :pc accumulator :accumulator log :log} state
        [operation argument] (nth instructions pc)
        next-state (merge state {:pc (inc pc) :log (conj log pc)})]
    (merge next-state
           (case operation
             "acc" {:accumulator (+ accumulator argument)}
             "nop" {}
             "jmp" {:pc (+ pc argument)}))))

(defn run
  ([instructions] (run instructions initial-state))
  ([instructions state]
   (let [{pc :pc accumulator :accumulator log :log} state]
    (cond
      (some #{pc} log) [:loop accumulator]
      (>= pc (count instructions)) [:halt accumulator]
      :else (run instructions (step instructions state))))))

(defn program-permutations
  ([] (program-permutations 0))
  ([n] (let [[o a] (nth program n)
             flipped ({"acc" "acc", "jmp" "nop", "nop" "jmp"} o)]
         (lazy-seq
          (cons (assoc program n [flipped a])
                (program-permutations (inc n)))))))

(println "Part 1:" (run program))

(println "Part 2:" (->> (map run (program-permutations))
                        (drop-while #(= :loop (first %)))
                        (first)))
