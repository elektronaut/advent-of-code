(require '[clojure.string :as str])

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
  (println cwd)
  (case (command :name)
    "cd" [(cd cwd (command :arg)) tree]
    "ls" [cwd (assoc-in tree cwd (ls (command :output)))]))

(def tree
  (->> (str/split (slurp "input.txt") #"\n?\$ ")
       (drop 1)
       (map parse-command)
       (reduce step [["/"] {}])
       (last)))
