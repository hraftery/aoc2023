(require '[clojure.string :as str])
;(require '[clojure.set :as s])
;(require '[clojure.math.numeric-tower :as m])
;(require ['clojure.math.combinatorics :refer :all])


;;;;;;;;
; Util
;;;;;;;;

(defn sum [seq] (reduce + seq))
(defn prod [seq] (reduce * seq))
(defn parse-int [s] (Integer/parseInt s))
(defn is-digit [c] (Character/isDigit c))
(defn mapmap [f m] (map (fn [mm] (map f mm)) m))
(defn str2nums [s] (map read-string (str/split s #"\s+")))
(defn csv2nums [s] (map parse-int (str/split s #",")))
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
(defn enumerate [l] (map-indexed list l))
(defn range-from [start] (drop-while #(> start %) (range)))
(defn col-count [m] (count (get m 0)))
(defn col-count-list [m] (count (nth m 0)))
(defn row-count [m] (count m))
(defn mget [m x y] (get (get m y) x))
(defn count-less-than [coll x] (count (filter #(< % x) coll)))
;(defn pairs [coll] (combinations coll 2))
(defn absolute-difference [a b] (if (> a b) (- a b) (- b a)))
(defn manhatten-distance [p0 p1]
  (let [[x0 y0] p0, [x1 y1] p1]
    (+ (absolute-difference x0 x1) (absolute-difference y0 y1))))
(defn replace-at [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))
;(def conj-unless-nil ((remove nil?) conj)) ; "transducer" version. Doesn't do arity variants.
(defn conj-unless-nil
  ([coll x] (if x (conj coll x) coll))
  ([coll x & xs] (if (= 1 (count xs)) ; jesus, this was tricky. The conj source led me astray.
                   (conj-unless-nil (conj-unless-nil coll x) (first xs))
                   (conj-unless-nil (conj-unless-nil coll x) (first xs) (next xs)))))
(defn zip [coll1 coll2] (map vector coll1 coll2))
#_(defn xor [set1 set2] (s/difference (s/union set1 set2)
                                   (s/intersection set1 set2)))
(defn nth-swap [index coll] (nth coll index)) ; argument order like get


;;;;;;;;;;
; Part 1
;;;;;;;;;;

;(def ^:const fname "example.txt")
(def ^:const fname "input.txt")


(defn parse-input [fname]
  (let [m (str/split-lines (slurp fname))]
    {:cubePts (set (for [[y row] (enumerate m)
                         [x cell] (enumerate row)
                         :when (= cell \#)]
                       (list x y)))
     :roundPts (set (for [[y row] (enumerate m)
                          [x cell] (enumerate row)
                          :when (= cell \O)]
                      (list x y)))
     :numRows (row-count m) ; iterate *again* to get number of rows
     :numCols (col-count m)})) ; iterate one more time to get number of cols!

(defn tilt-north [input]
  (let [{:keys [cubePts roundPts]} input
        roundPtsOrdered (sort-by second roundPts)
        f (fn [acc roundPt]
            (let [[x y] roundPt
                  rocksInCol (filter #(= x (first %)) (concat acc cubePts))
                  rocksAbove (filter #(< (second %) y) rocksInCol)
                  maxY (apply max -1 (map second rocksAbove))]
              (conj acc (list x (inc maxY)))))
        roundPtsTilted (reduce f '() roundPtsOrdered)]
    (assoc input :roundPts (set roundPtsTilted))))

(defn draw-platform [input]
  (let [{:keys [cubePts roundPts numRows numCols]} input
        do-row (fn [y] (map #(cond
                               (contains? cubePts (list % y)) \#
                               (contains? roundPts (list % y)) \O
                               :else \.)
                            (range numCols)))]
    (map #(apply str (do-row %)) (range numRows))))

(defn weigh-platform [input]
  (let [{:keys [roundPts numRows]} input]
    (sum (map #(- numRows (second %)) roundPts))))

(defn part1 []
  (->> (parse-input fname)
       tilt-north
       weigh-platform))

;(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn tilt [dir input]
  (let [{:keys [cubePts roundPts numRows numCols]} input
        roundPtsRowOrdered (sort-by second roundPts)
        roundPtsColOrdered (sort-by first roundPts)
        roundPtsOrdered (case dir
                          :north roundPtsRowOrdered
                          :west roundPtsColOrdered
                          :south (reverse roundPtsRowOrdered)
                          :east (reverse roundPtsColOrdered))
        f (fn [acc roundPt]
            (let [[x y] roundPt
                  rocksInCol (filter #(= x (first %)) (concat acc cubePts))
                  rocksInRow (filter #(= y (second %)) (concat acc cubePts))
                  rocksAbove (filter #(< (second %) y) rocksInCol)
                  rocksLeft (filter #(< (first %) x) rocksInRow)
                  rocksBelow (filter #(> (second %) y) rocksInCol)
                  rocksRight (filter #(> (first %) x) rocksInRow)
                  maxY (apply max -1 (map second rocksAbove))
                  maxX (apply max -1 (map first rocksLeft))
                  minY (apply min numRows (map second rocksBelow))
                  minX (apply min numCols (map first rocksRight))]
              (case dir
                :north (conj acc (list x (inc maxY)))
                :west (conj acc (list (inc maxX) y))
                :south (conj acc (list x (dec minY)))
                :east (conj acc (list (dec minX) y)))))
        roundPtsTilted (reduce f '() roundPtsOrdered)]
    (assoc input :roundPts (set roundPtsTilted))))

(defn tilt-cycle [input]
  (tilt :east (tilt :south (tilt :west (tilt :north input)))))
             
(defn part2 []
  (let [allPatterns (->> (parse-input fname)
                         (iterate tilt-cycle)
                         enumerate)
        f (fn [acc x]
            (let [[i pattern] x]
              (if (contains? acc pattern)
                (reduced (list (get acc pattern) i))
                (assoc acc pattern i))))
        firstCycle (reduce f {} allPatterns)
        [cStart cEnd] firstCycle
        m (mod (- 1000000000 cStart) (- cEnd cStart))
        thePattern (second (nth allPatterns (+ cStart m)))]
    (println cStart cEnd)
    (weigh-platform thePattern)))

(println (part2))

;; (->> (parse-input fname)
;;      (iterate tilt-cycle)
;;      enumerate
;;      (nth-swap 100)
;;      second
;;      draw-platform)