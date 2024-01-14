(require '[clojure.string :as str])
;(require '[clojure.set :as s])
;(require '[clojure.math.numeric-tower :as m])

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
(defn index-of [elem coll]
  (first (keep-indexed #(if (= elem %2) %1)
                       coll)))
(defn positions [pred coll]
  (keep-indexed (fn [idx x] (when (pred x) idx))
                coll))
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
; Part 1
;;;;;;;;;;

;(def ^:const fname "example.txt")
;(def ^:const fname "example2.txt")
(def ^:const fname "input.txt")


(defn parse-input [fname]
  (->> (slurp fname)
       str/split-lines
       (map str2nums)))
(parse-input fname)

(defn get-diff [step]
  (->> step
       (partition 2 1)
       (map reverse)
       (map (partial apply -))))
(get-diff '(0 3 7 9 12 15))


(defn make-sequences [history]
  (loop [historyList (list history)]
    (let [currStep (first historyList)
          nextStep (get-diff currStep)
          nextHistory (conj historyList nextStep)]
      ;(println nextStep)
      (if (every? zero? nextStep)
        nextHistory
        (recur nextHistory)))))
(make-sequences '(0 3 6 9 12 15))

(defn make-prediction [history]
  (->> history
       make-sequences
       rest ; ignore the zeros
       (map last)
       sum));
(make-prediction '(0 3 6 9 12 15))

(defn part1 []
  (let [input (parse-input fname)]
    (->> input
         (map make-prediction)
         sum)))

;(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn make-reverse-prediction [history]
  (->> history
       make-sequences
       rest ; ignore the zeros
       (map first)
       (reduce #(- %2 %1))));
(make-reverse-prediction '(10  13  16  21  30  45))

(defn part2 []
  (let [input (parse-input fname)]
    (->> input
         (map make-reverse-prediction)
         sum)))

(println (part2))
