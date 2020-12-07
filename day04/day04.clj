(require '[clojure.string :as str])

(def passports
  (map #(apply hash-map (str/split % #"[^\w\d#]+"))
       (str/split (slurp "input.txt") #"\n\n")))

(def required-fields
  ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])

(defn required-fields? [p]
  (= (count (keep p required-fields))
     (count required-fields)))

(defn in-range? [min max s]
  (let [n (Integer/parseInt s)]
    (and (>= n min)
         (<= n max))))

(defn valid-height? [s]
  (let [value (re-find #"^[\d]+" s)
        unit (re-find #"[^\d]+$" s)]
    (case unit
      "cm" (in-range? 150 193 value)
      "in" (in-range? 59 76 value)
      nil)))

(defn valid-passport? [p]
  (and (required-fields? p)
       (in-range? 1920 2002 (p "byr"))
       (in-range? 2010 2020 (p "iyr"))
       (in-range? 2020 2030 (p "eyr"))
       (valid-height? (p "hgt"))
       (re-matches #"#[\d\w]{6}" (p "hcl"))
       (some #{(p "ecl")} ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])
       (re-matches #"[\d]{9}" (p "pid"))))

(println "Part 1:"
         (count (filter required-fields? passports)))

(println "Part 2:"
         (count (filter valid-passport? passports)))
