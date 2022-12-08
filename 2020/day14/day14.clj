(ns advent-of-code.2020.14
  (:require [clojure.string :as str]))

(defn parse-command [str]
  (let [[cmd value] (str/split str #" = ")]
    (if (= cmd "mask")
      [:mask value]
      [:mem (Long/parseLong (re-find #"[\d]+" cmd)) (Long/parseLong value)])))

(defn bits [n]
  (map #(if (bit-test n (- 35 %)) \1 \0) (range 0 36)))

(defn value-mask [mask num]
  (let [mask1 (Long/parseLong (str/replace mask "X" "0") 2)
        mask0 (Long/parseLong (str/replace mask "X" "1") 2)]
    (bit-and (bit-or num mask1) mask0)))

(defn addr-permutations [addrs [m a]]
  (case m
    \1 (map #(conj % m) addrs)
    \0 (map #(conj % a) addrs)
    \X (concat (addr-permutations addrs [\0 \0])
               (addr-permutations addrs [\0 \1]))))

(defn addresses [addr mask]
  (->> (map vector mask (bits addr))
       (reduce addr-permutations [[]])
       (map #(Long/parseLong (str/join %) 2))))

(defn read-program [file]
  (->> (slurp file)
       (str/split-lines)
       (map parse-command)))

(defn run [eval-fn program]
  (->> (reduce eval-fn {:mem {} :mask ""} program)
       (:mem)
       (map val)
       (reduce +)))

(defn eval-part1 [{mask :mask mem :mem} [instruction & args]]
  (merge
   {:mask mask :mem mem}
   (case instruction
     :mask {:mask (first args)}
     :mem {:mem (assoc mem (first args)
                       (value-mask mask (second args)))})))

(defn eval-part2 [{mask :mask mem :mem} [instruction & args]]
  (merge
   {:mask mask :mem mem}
   (case instruction
     :mask {:mask (first args)}
     :mem (let [[addr value] args]
            {:mem (merge mem (reduce (fn [m a] (assoc m a value)) {}
                                     (addresses addr mask)))}))))

(let [program (read-program "2020/day14/input.txt")]
  (println "Part 1:" (run eval-part1 program))
  (println "Part 2:" (run eval-part2 program)))
