(require '[clojure.string :as str])
;(require '[clojure.set :as s])
;(require '[clojure.math.numeric-tower :as m])
;(require ['clojure.math.combinatorics :refer :all])


;;;;;;;;
; Util
;;;;;;;;

(defn sum [seq] (reduce + seq))
(defn prod [seq] (reduce * seq))
(defn parse-int-throws [s] (Integer/parseInt s))
(defn parse-int [s] (try (parse-int-throws s)
                         (catch Exception _ nil)))
(defn is-digit [c] (Character/isDigit c))
(defn mapmap [f m] (map (fn [mm] (map f mm)) m))
(defn str2nums [s] (map read-string (str/split s #"\s+")))
(defn csv2nums [s] (map parse-int (str/split s #",")))
(defn str-filter [pred s] (apply str (filter pred s)))
(defn list-insert [lst elem index]
  (let [[l r] (split-at index lst)]
    (concat l [elem] r)))
; Note if oldElem can't be found, newElem gets appended.
(defn list-replace [lst oldElem newElem]
  (let [[l r] (split-with (complement #{oldElem}) lst)]
    (concat l [newElem] (rest r))))
; Note if pred isn't satisfied, newElem gets appended.
(defn list-replace-with [lst pred newElem]
  (let [[l r] (split-with (complement pred) lst)]
    (concat l [newElem] (rest r))))
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
(defn enumerate-1-based [l] (rest (enumerate (conj l 0))))
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
    {:mirrorPts (into {} (for [[y row] (enumerate m)
                               [x cell] (enumerate row)
                               :when (not (= cell \.))]
                           [(list x y) cell]))
     :numRows (row-count m) ; iterate *again* to get number of rows
     :numCols (col-count m)})) ; iterate one more time to get number of cols!

(defn step-vec [beamVec]
  (let [[[x y] dir] beamVec
        newPt (case dir
                :up (list x (dec y))
                :right (list (inc x) y)
                :down (list x (inc y))
                :left (list (dec x) y))]
    [newPt dir]))

(defn reflect-vec [mirror beamVec]
  (let [[pt dir] beamVec
        newDir (case mirror
                 \\ (case dir :up :left, :right :down, :down :right, :left :up)
                 \/ (case dir :up :right, :right :up, :down :left, :left :down))]
    [pt newDir]))

(defn off-chart? [beamVec layout]
  (let [[[x y] _] beamVec
        {:keys [numRows numCols]} layout]
    (or (< x 0) (< y 0) (>= x numCols) (>= y numRows))))

(defn trace-beam [beamVec layout]
  (let [{:keys [mirrorPts energisedPts]} layout
        newBeamVec (step-vec beamVec)
        [pt dir] newBeamVec
        newEnergistedPts (conj energisedPts newBeamVec)
        newLayout (assoc layout :energisedPts newEnergistedPts)]
    ;(println beamVec energisedPts)
    (if (or
         (off-chart? newBeamVec layout)
         (contains? energisedPts newBeamVec)) ; no need to do it again
      layout
      (case (get mirrorPts pt)
        nil (trace-beam newBeamVec newLayout)
        \\ (trace-beam (reflect-vec \\ newBeamVec) newLayout)
        \/ (trace-beam (reflect-vec \/ newBeamVec) newLayout)
        \- (if (some #{dir} '(:left :right))
             (trace-beam newBeamVec newLayout)
             (trace-beam (reflect-vec \/ newBeamVec)
                         (trace-beam (reflect-vec \\ newBeamVec) newLayout)))
        \| (if (some #{dir} '(:up :down))
             (trace-beam newBeamVec newLayout)
             (trace-beam (reflect-vec \/ newBeamVec)
                         (trace-beam (reflect-vec \\ newBeamVec) newLayout)))
      ))))


(defn consolidate-points [energisedPts]
  (set (map first energisedPts)))

(defn part1 []
  (->> (parse-input fname)
       (#(assoc %1 :energisedPts #{}))
       (trace-beam ['(-1 0) :right])
       :energisedPts
       consolidate-points
       count))

;(println (part1))


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn part2 []
  (let [input (assoc (parse-input fname) :energisedPts #{})
        {:keys [numRows numCols]} input
        startVecsL (for [y (range numRows)] [(list -1 y) :right])
        startVecsR (for [y (range numRows)] [(list numCols y) :left])
        startVecsT (for [x (range numCols)] [(list x -1) :down])
        startVecsB (for [x (range numCols)] [(list x numRows) :up])
        startVecs (concat startVecsL startVecsR startVecsT startVecsB)
        f (fn [startVec] (->> (trace-beam startVec input)
                              :energisedPts
                              consolidate-points
                              count))]
    (apply max (map f startVecs))))


(println (part2))
