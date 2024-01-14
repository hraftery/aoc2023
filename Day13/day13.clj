(require '[clojure.string :as str])
(require '[clojure.set :as s])
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
(defn xor [set1 set2] (s/difference (s/union set1 set2)
                                   (s/intersection set1 set2)))


;;;;;;;;;;
; Part 1
;;;;;;;;;;

;(def ^:const fname "example.txt")
(def ^:const fname "input.txt")

(defn parse-pattern [m]
  {:rockPts (set (for [[y row] (enumerate m)
                       [x cell] (enumerate row)
                       :when (= cell \#)]
                     (list x y)))
     :numRows (row-count m) ; iterate *again* to get number of rows
     :numCols (col-count-list m)}) ; iterate one more time to get number of cols!

(defn get-rows-and-cols [input]
  (let [{:keys [rockPts numRows numCols]} input
        byRows (for [rowi (range numRows)]
                 (map first
                      (filter #(= rowi (second %)) rockPts)))
        byCols (for [coli (range numCols)]
                 (map second
                      (filter #(= coli (first %)) rockPts)))]
    (assoc input :rows (map set byRows)
                 :cols (map set byCols))))

(defn parse-input [fname]
  (->> fname
       slurp
       str/split-lines
       (partition-by str/blank?)
       (take-nth 2)
       (map parse-pattern)
       (map get-rows-and-cols)))
(parse-input fname)

(defn find-symmetry [rowsOrCols numRowsOrCols]
  (loop [i 1, left '()]
    (if (= i numRowsOrCols)
      nil
      (let [newLeftPre (conj left (nth rowsOrCols (dec i)))
            newLeft (cond
                      (> (* 2 i) (inc numRowsOrCols)) (butlast (butlast newLeftPre))
                      (= (* 2 i) (inc numRowsOrCols)) (butlast newLeftPre)
                      :else                     newLeftPre)
            right (take i (drop i rowsOrCols))]
        (println newLeft right)
        (if (= newLeft right)
          i
          (recur (inc i) newLeft))))))
(find-symmetry '(#{0 7 6 3 2} #{7 4 2 5} #{0 1 8} #{0 1 8} #{7 4 2 5} #{7 6 3 2} #{0 7 4 2 5})
               7)
(find-symmetry '(#{0 6 3 2} #{3 2} #{0 1 4 6 5} #{0 5} #{1 4 6} #{1 4 6} #{0 5} #{0 1 4 6 5} #{3 2})
               9)

(defn calc-symmetry-score
  ([pattern] (calc-symmetry-score find-symmetry pattern))
  ([find-symmetry-func pattern]
   (let [{:keys [numRows rows numCols cols]} pattern
         rowMirror (find-symmetry-func rows numRows)
         colMirror (find-symmetry-func cols numCols)]
    ;(println rowMirror colMirror)
     (if rowMirror (* 100 rowMirror) colMirror))))

(defn part1 []
  (->> (parse-input fname)
       (map calc-symmetry-score)
       sum))

;(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn is-symmetrical-bar-one [left right]
  (loop [i 0, diffCount 0, coll (zip left right)]
    (if (= i (count coll))
      (= diffCount 1)
      (let [[l r] (nth coll i)
            diff (xor l r)
            newDiffCount (+ diffCount (count diff))]
        ;(println l r diff)
        (if (<= newDiffCount 1)
          (recur (inc i) newDiffCount coll)
          false)))))
(is-symmetrical-bar-one '(#{1 4 6} #{0 5} #{0 1 4 6 5} #{3 2}) '(#{1 4 6} #{0 5} #{0 1 4 6 5} #{3 2}))
(is-symmetrical-bar-one '(#{0 1 8} #{7 4 2 5} #{0 7 6 3 2}) '(#{0 1 8} #{7 4 2 5} #{7 6 3 2}))
(is-symmetrical-bar-one '(#{0 7 1 4 3 2} #{1 4 6 12 2 11 9} #{1 3 9 5 10} #{4 6 2 11 9 5 10 8} #{0 7 1 6 3 11 10} #{12 2 9 5 8})
                        '(#{0 7 1 4 3 2} #{1 4 6 12 2 11 9} #{1 3 9 5 10 8} #{4 6 2 11 9 5 10 8} #{0 7 1 6 3 11 10} #{12 2 9 5 8}))

(defn find-symmetry-with-smudge [rowsOrCols numRowsOrCols]
  (loop [i 1, left '()]
    (if (= i numRowsOrCols)
      nil
      (let [newLeftPre (conj left (nth rowsOrCols (dec i)))
            newLeft (cond
                      (> (* 2 i) (inc numRowsOrCols)) (butlast (butlast newLeftPre))
                      (= (* 2 i) (inc numRowsOrCols)) (butlast newLeftPre)
                      :else                     newLeftPre)
            right (take i (drop i rowsOrCols))]
        ;(println newLeft right)
        (if (is-symmetrical-bar-one newLeft right)
          i
          (recur (inc i) newLeft))))))
(find-symmetry-with-smudge '(#{0 7 6 3 2} #{7 4 2 5} #{0 1 8} #{0 1 8} #{7 4 2 5} #{7 6 3 2} #{0 7 4 2 5})
                           7)

(defn part2 []
  (->> (parse-input fname)
       #_#_second
       (calc-symmetry-score find-symmetry-with-smudge)
       (map (partial calc-symmetry-score find-symmetry-with-smudge))
       sum))

(part2)
