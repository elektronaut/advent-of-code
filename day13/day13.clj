(require '[clojure.string :as str])

(def lines (str/split-lines (slurp "input.txt")))

(def timestamp (Integer/parseInt (first lines)))

(def buses (->> (str/split (last lines) #",")
                (map-indexed (fn [i n] {:interval n :offset i}))
                (filter #(re-find #"[\d]+" (% :interval)))
                (map #(merge % {:interval (bigint (Long/parseLong (% :interval)))}))))

(defn abs [n]
  (if (neg? n) (- 0 n) n))

(defn next-departure [time {interval :interval}]
  (* (int (Math/ceil (/ time interval))) interval))

(let [next-bus (first (sort-by #(next-departure timestamp %) buses))
      departure (next-departure timestamp next-bus)]
  (println "Part 1:" (* (- departure timestamp) (next-bus :interval))))

(defn extended-gcd [a b]
  (loop [s 0 s0 1 t 1 t0 0
         r (abs b)
         r0 (abs a)]
    (if (zero? r)
      [r0 s0]
      (let [q (quot r0 r)]
        (recur (- s0 (* q s)) s
               (- t0 (* q t)) t
               (- r0 (* q r)) r)))))

(defn invmod [e, et]
  (mod (second (extended-gcd e et)) et))

(defn chinese-remainder [mods remainders]
  (let [max (reduce * mods)]
    (mod (->> (map vector mods remainders)
              (map (fn [[m r]] (* r max (/ (invmod (/ max m) m) m))))
              (reduce +))
         max)))

(println "Part 2:"
         (chinese-remainder
          (map :interval buses)
          (map (fn [{r :offset n :interval}] (- n (mod r n))) buses)))
