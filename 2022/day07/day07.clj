(ns advent-of-code.2022.07
  (:require [clojure.string :as str]))

(defn parse-command [string]
  (let* [lines (str/split-lines string)
         command (str/split (first lines) #" ")
         name (first command)
         arg (str/join " " (rest command))
         output (drop 1 lines)]
    {:name name :arg arg :output output}))

(defn cd [path arg]
  (case arg
    "/" ["/"]
    ".." (pop path)
    (conj path arg)))

(defn ls-line [line]
  (let [[value-str name] (str/split line #" ")]
    {name (if (= "dir" value-str) {} (Integer/parseInt value-str))}))

(defn ls [output]
  (reduce merge (map ls-line output)))

(defn step [[cwd tree] command]
  (case (command :name)
    "cd" [(cd cwd (command :arg)) tree]
    "ls" [cwd (assoc-in tree cwd (ls (command :output)))]))

(def tree
  (->> (str/split (slurp "2022/day07/input.txt") #"\n?\$ ")
       (drop 1)
       (map parse-command)
       (reduce step [["/"] {}])
       (last)))

(defn directories [node]
  (let [subdirs (filter map? (vals node))]
    (conj (mapcat directories subdirs) node)))

(defn directory-size [node]
  (let [subdirs (filter map? (vals node))
        size (reduce + (remove map? (vals node)))]
    (+ size (reduce + (map directory-size subdirs)))))

(def sizes
  (->> (directories (get-in tree ["/"]))
       (map directory-size)
       (sort)))

(println "Part 1:"
         (->> (filter #(<= % 100000) sizes)
              (reduce +)))

(println "Part 2:"
         (let* [root-size (last sizes)
                free-space (- 70000000 root-size)
                required-size (- 30000000 free-space)]
           (first (filter #(>= % required-size) sizes))))
