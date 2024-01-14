(require '[clojure.string :as str])

;(def ^:const fname "example.txt")
;(def ^:const fname "example2.txt")
(def ^:const fname "input.txt")

(defn sum [seq] (reduce + seq))

; Part 1
(defn parse-int [s] (Integer/parseInt s))
(defn first-digit [s] (second (re-matches #".*?(\d).*" s)))
(defn last-digit [s] (second (re-matches #".*(\d).*" s)))

(->> (slurp fname)
     str/split-lines
     (map #(vector (first-digit %) (last-digit %)))
     (map (comp parse-int str/join))
     sum)

; Part 2
(defn parse-int2 [s]
  (case s
    ("one"   "1") 1
    ("two"   "2") 2
    ("three" "3") 3
    ("four"  "4") 4
    ("five"  "5") 5
    ("six"   "6") 6
    ("seven" "7") 7
    ("eight" "8") 8
    ("nine"  "9") 9))
(defn first-digit2 [s]
  (second (re-matches #".*?(one|two|three|four|five|six|seven|eight|nine|\d).*" s)))
(defn last-digit2 [s]
  (second (re-matches #".*(one|two|three|four|five|six|seven|eight|nine|\d).*" s)))

(->> (slurp fname)
     str/split-lines
     (map #(vector (first-digit2 %) (last-digit2 %)))
     (map #(map parse-int2 %))
     (map #(+ (* 10 (first %)) (second %)))
     sum)