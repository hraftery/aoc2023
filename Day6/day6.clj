(require '[clojure.string :as str])
;(require '[clojure.set :as s])
;(require '[clojure.math :as m])

;;;;;;;;
; Util
;;;;;;;;

(defn sum [seq] (reduce + seq))
(defn prod [seq] (reduce * seq))
(defn parse-int [s] (Integer/parseInt s))
(defn is-digit [c] (Character/isDigit c))
(defn mapmap [f m] (map (fn [mm] (map f mm)) m))
(defn str2nums [s] (map read-string (str/split s #"\s+")))
(defn str-filter [pred s] (apply str (filter pred s)))
(defn list-insert [lst elem index]
  (let [[l r] (split-at index lst)]
    (concat l [elem] r)))
(defn find-first [f coll]
  (first (filter f coll)))
(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) nil)
       (cons (first s) (take-until pred (rest s)))))))


;;;;;;;;;;
; Impl 1
;;;;;;;;;;

;(def ^:const fname "example.txt")
;(def ^:const fname "example2.txt")
(def ^:const fname "input.txt")

(defn parse-input [fname]
  (->> (slurp fname)
       str/split-lines
       (map #(str/split % #":\s+"))
       (map second)
       (map str2nums)))


;;;;;;;;;;
; Part 1
;;;;;;;;;;

(defn resulting-distance [duration holdTime]
  (* (- duration holdTime) holdTime))
(resulting-distance 7 2)

(defn num-ways-to-beat-record [duration record]
  (->> (range 1 duration) ; candidate hold times (0 and max excluded because they always result in 0 distance)
       (map (partial resulting-distance duration))
       (filter (partial < record))
       count))
(num-ways-to-beat-record 7 9)

(defn part1 []
  (let [[durations records] (parse-input fname)]
    ;(println {:durations durations :records records})
    (->> (zipmap durations records)
         (map (partial apply num-ways-to-beat-record))
         prod)))

;(part1)


;;;;;;;;;;
; Impl 2
;;;;;;;;;;


;;;;;;;;;;
; Part 2
;;;;;;;;;;


(defn parse-input2 [fname]
  (->> (slurp fname)
       str/split-lines
       (map (partial str-filter is-digit))
       (map read-string)))

(defn part2 []
  (let [[duration record] (parse-input2 fname)]
    (num-ways-to-beat-record duration record)))

(println (part2))
