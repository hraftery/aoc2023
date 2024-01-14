(require '[clojure.string :as str])
(require '[clojure.set :as s])
;(require '[clojure.math.numeric-tower :as m])
;(require ['clojure.math.combinatorics :refer :all])


;;;;;;;;
; Util
;;;;;;;;

(defn sum [seq] (reduce + seq))
(defn prod [seq] (reduce * seq))
(defn parse-int-throws ([s] (Integer/parseInt s))
                       ([s base] (Integer/parseInt s base)))
(defn parse-int
  ([s] (try (parse-int-throws s) (catch Exception _ nil)))
  ([s base] (try (parse-int-throws s base) (catch Exception _ nil))))
(defn char2int [c] (- (int c) (int \0)))
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
(defn find-first-key [f coll]
  (find-first f (keys coll)))
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
(defn seq-contains? [coll elem] (some? (some #{elem} coll)))
(defn get-or-throw [coll key]
  (if (contains? coll key)
    (get coll key)
    (let [msg (format "Key not found: %s" key)]
      (throw (Exception. msg)))))
(defn firsts [coll] ; like Haskell's "tails": (firsts [1 2 3 4]) => ([1] [1 2] [1 2 3] [1 2 3 4])
  (rest (reductions (fn [acc x] (conj acc x)) [] coll)))
(defn fmap [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))
(defn fmapkv [f m]
  (into (empty m) (for [[k v] m] [k (f k v)])))
(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))
(defmethod print-method clojure.lang.PersistentQueue
  [q ^java.io.Writer w]
  (.write w "#queue ")
  (print-method (sequence q) w))


;;;;;;;;;;
; Part 1
;;;;;;;;;;

;(def ^:const fname "example.txt")
(def ^:const fname "input.txt")

(defn parse-input [fname]
  (let [m (str/split-lines (slurp fname))
        cells (into {}
                    (for [[y row] (enumerate m)
                          [x cell] (enumerate row)]
                      [(list x y) cell]))]
    {:rocks (set (keys (filter #(= (second %) \#) cells)))
     :plots (set (keys (filter #(or (= (second %) \.)
                                    (= (second %) \S)) cells)))
     :startPt (first (keys (filter #(= (second %) \S) cells))) ; iterate *again* just to get the start point
     :numRows (row-count m) ; iterate *yet again* to get number of rows
     :numCols (col-count m)})) ; iterate one more time to get number of cols!
;(parse-input fname)

(defn neighbours [garden pt]
  (let [[x y] pt
        plots (garden :plots)]
    (filter (partial contains? plots)
            [(list (dec x) y) (list x (dec y))
             (list (inc x) y) (list x (inc y))])))
;(neighbours (parse-input fname) '(0 0))

(defn take-step [garden startPts]
  (->> startPts
       (map (partial neighbours garden))
       (apply concat)
       set))

(defn take-steps [garden]
  (iterate (partial take-step garden) (list (garden :startPt))))

(defn part1 []
  (let [garden (parse-input fname)]
    (->> 64
         (nth (take-steps garden))
         count)))

;(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn wrap-point [garden [x y]]
  (let [{:keys [numRows numCols]} garden]
    (list (mod x numCols) (mod y numRows))))
;(wrap-point (parse-input fname) '(-1 -1))

(defn neighbours2 [garden pt]
  (let [[x y] pt
        plots (garden :plots)]
    (filter #(contains? plots (wrap-point garden %))
            [(list (dec x) y) (list x (dec y))
             (list (inc x) y) (list x (inc y))])))

(defn take-step2 [garden startPts]
  (->> startPts
       (map (partial neighbours2 garden))
       (apply concat)
       set))

(defn take-steps2 [garden]
  (iterate (partial take-step2 garden) (list (garden :startPt))))

(defn part2 []
  (let [garden (parse-input fname)]
     (->> (+ 65 131 131 131)
          (nth (take-steps2 garden))
          count)))

(println (part2))
