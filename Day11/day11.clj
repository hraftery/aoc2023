(require '[clojure.string :as str])
;(require '[clojure.set :as s])
;(require '[clojure.math.numeric-tower :as m])
(require ['clojure.math.combinatorics :refer :all])


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
(defn enumerate [l] (map-indexed list l))
(defn range-from [start] (drop-while #(> start %) (range)))
(defn col-count [m] (count (get m 0)))
(defn row-count [m] (count m))
(defn mget [m x y] (get (get m y) x))
(defn count-less-than [coll x] (count (filter #(< % x) coll)))
(defn pairs [coll] (combinations coll 2))
(defn absolute-difference [a b] (if (> a b) (- a b) (- b a)))
(defn manhatten-distance [p0 p1]
  (let [[x0 y0] p0, [x1 y1] p1]
    (+ (absolute-difference x0 x1) (absolute-difference y0 y1))))

;;;;;;;;;;
; Part 1
;;;;;;;;;;

;(def ^:const fname "example.txt")
(def ^:const fname "input.txt")

(defn parse-input [fname]
  (let [m (str/split-lines (slurp fname))]
    {:galaxyPts (set (for [[y row] (enumerate m)
                          [x cell] (enumerate row)
                          :when (= cell \#)]
                         (list x y)))
     :numRows (row-count m) ; iterate *again* to get number of rows
     :numCols (col-count m)})) ; iterate one more time to get number of cols!
(parse-input fname)

(defn galaxies-in-col [galaxyPts x] (filter #(= x (first %)) galaxyPts))
(defn galaxies-in-row [galaxyPts y] (filter #(= y (second %)) galaxyPts))
(defn is-no-galaxy-in-col [galaxyPts x] (nil? (seq (galaxies-in-col galaxyPts x))))
(defn is-no-galaxy-in-row [galaxyPts y] (nil? (seq (galaxies-in-row galaxyPts y))))

(defn expand-image [image]
  (let [{:keys [galaxyPts numRows numCols]} image
        emptyCols (filter #(is-no-galaxy-in-col galaxyPts %) (range 0 numCols))
        emptyRows (filter #(is-no-galaxy-in-row galaxyPts %) (range 0 numRows))
        newPts (map #(apply
                      (fn [x y] [(+ x (count-less-than emptyCols x))
                                 (+ y (count-less-than emptyRows y))])
                      %) galaxyPts)]
    (assoc image :galaxyPts (set newPts)
                 :numCols (+ numCols (count emptyCols))
                 :numRows (+ numRows (count emptyRows)))))

(defn draw-image [image]
  (let [{:keys [galaxyPts numRows numCols]} image
        do-col (fn [y] (map #(if (contains? galaxyPts (list % y)) \# \.)
                              (range numCols)))]
    (map #(apply str (do-col %)) (range numRows))))


(defn part1 []
  (->> (parse-input fname)
       expand-image
       #_draw-image
       :galaxyPts
       pairs
       (map (partial apply manhatten-distance))
       sum))

(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn expand-image-by [by image]
  (let [{:keys [galaxyPts numRows numCols]} image
        emptyCols (filter #(is-no-galaxy-in-col galaxyPts %) (range 0 numCols))
        emptyRows (filter #(is-no-galaxy-in-row galaxyPts %) (range 0 numRows))
        newPts (map #(apply
                      (fn [x y] [(+ x (* by (count-less-than emptyCols x)))
                                 (+ y (* by (count-less-than emptyRows y)))])
                      %) galaxyPts)]
    (assoc image :galaxyPts (set newPts)
           :numCols (+ numCols (* by (count emptyCols)))
           :numRows (+ numRows (* by (count emptyRows))))))


(defn part2 []
  (->> (parse-input fname)
       (expand-image-by 999999)
       #_draw-image
       :galaxyPts
       pairs
       (map (partial apply manhatten-distance))
       sum))

(part2)
