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
(defn list-contains? [coll elem] (some? (some #{elem} coll)))
(defn get-or-throw [coll key]
  (if (contains? coll key)
    (get coll key)
    (let [msg (format "Key not found: %s" key)]
      (throw (Exception. msg)))))

;;;;;;;;;;
; Part 1
;;;;;;;;;;

;(def ^:const fname "example.txt")
(def ^:const fname "input.txt")

(defn move
  ([pt dir]     (let [[x y] pt]
                  (case dir
                    :up    (list x (dec y))
                    :right (list (inc x) y)
                    :down  (list x (inc y))
                    :left  (list (dec x) y))))
  ([pt dir len] (let [[x y] pt]
                  (case dir
                    :up    (list x (- y len))
                    :right (list (+ x len) y)
                    :down  (list x (+ y len))
                    :left  (list (- x len) y)))))

(defn parse-line [l]
  (let [[dirStr lenStr colourStr] (str/split l #" ")
        dir (get {"U" :up, "R" :right, "D" :down, "L" :left} dirStr)
        len (parse-int lenStr)
        colour (parse-int (subs colourStr 2 8) 16)]
    (list dir len colour)))
(parse-line "R 6 (#70c710)")

(defn make-trench-edge [startPt dig]
  (let [[dir len colour] dig]
    (for [i (range len)]
       [(move startPt dir (inc i)) colour])))
;(make-trench-edge '(0 0) (list :right 6 8004067))

(defn make-trench [originPt digs]
  (let [f (fn [acc dig]
            (let [[accPts startPt] acc
                  [dir len colour] dig
                  newPts (make-trench-edge startPt dig)]
              (list
               (merge accPts (into {} newPts))
               (move startPt dir len))))]
    (first (reduce f (list {} originPt) digs))))
;(make-trench '(0 0) (list (list :right 4 44) (list :down 3 33)))

(defn extents [pts]
  (let [xs (map first pts)
        ys (map second pts)]
    [(apply min xs) (apply min ys)
     (apply max xs) (apply max ys)]))
;(extents (keys (parse-input fname)))

(defn points-by-row [pts]
  (let [f (fn [acc pt]
            (let [[x y] pt
                  row (get acc y #{})]
              (assoc acc y (conj row x))))]
  (reduce f {} pts)))
(points-by-row (list '(0 0) '(1 0) '(2 2) '(1 4)))

(defn vertical-walls [trenchPtsByRow]
  (let [f (fn [[y xs]]
            [y (s/intersection xs (get trenchPtsByRow (dec y)))])]
    (into {} (map f trenchPtsByRow))))

(defn parse-input [fname]
  (let [digs (->> (slurp fname)
                  (str/split-lines)
                  (map parse-line))
        trench (make-trench '(0 0) digs)
        trenchPts (keys trench)
        extents (extents trenchPts)
        trenchPtsByRow (points-by-row trenchPts)
        vertWalls (vertical-walls trenchPtsByRow)]
  {:digs digs, :trench trench, :extents extents,
   :trenchPtsByRow trenchPtsByRow, :vertWalls vertWalls}))
(parse-input fname)


(defn wrong-count-crossings [trenchPts ptsToCount]
  (let [runsOfSomeOrNone (partition-by (partial list-contains? trenchPts)
                                       ptsToCount)
        numRuns (count runsOfSomeOrNone)
        startsWithSome? (some #{(first ptsToCount)} trenchPts)]
    ;(println runsOfSomeOrNone)
    (if (even? numRuns)
      (/ numRuns 2)
      (if startsWithSome?
        (/ (inc numRuns) 2)
        (/ (dec numRuns) 2)))))
;; (count-crossings (list '(0 0) '(5 5) '(1 0) '(4 0) '(5 0) '(6 0))
;;                  (list '(0 0) '(1 0) '(2 0) '(3 0) '(4 0) '(5 0) '(6 0)))
;; (count-crossings (list        '(5 5) '(1 0) '(4 0) '(5 0) '(6 0))
;;                  (list '(0 0) '(1 0) '(2 0) '(3 0) '(4 0) '(5 0) '(6 0)))
;; (count-crossings (list '(5 5) '(2 0) '(3 0))
;;                  (list '(0 0) '(1 0) '(2 0) '(3 0) '(4 0) '(5 0) '(6 0)))

(defn count-crossings [trenchPts ptsToCount]
  (let [intersects (filter (partial list-contains? trenchPts)
                           ptsToCount)
        ; arbitrarily chose to look above to decide if it's a vertical wall and not just a corner or horizontal edge
        aboveIntersects (map #(list (first %) (dec (second %)))
                             intersects)
        walls (filter (partial list-contains? trenchPts)
                      aboveIntersects)]
    (count walls)))
(comment
  (let [tps (keys (parse-input fname))]
    (count-crossings tps (for [x (range 3)] (list x 1)))
    (count-crossings tps (for [x (range 4)] (list x 2)))
    (count-crossings tps (for [x (range 6)] (list x 6)))
    (count-crossings tps (for [x (range 1 7)] (list x 9)))))

(defn careful-inside? [trenchPts minX maxX pt]
  (let [[x y] pt
        leftPts (for [xi (range minX x)] (list xi y))
        rightPts (for [xi (range (inc x) (inc maxX))] (list xi y))
        leftCrossings (count-crossings trenchPts leftPts)
        rightCrossings (count-crossings trenchPts rightPts)]
    (if (odd? (+ leftCrossings rightCrossings))
      (let [msg (format "Must be even number of crossings: %s" pt)]
        (throw (Exception. msg)))
      (odd? leftCrossings))))

(defn scan-inside? [trenchPts minX maxX pt]
  (let [[x y] pt
        goLeft? (< (- x minX) (- maxX x))
        sidePts (if goLeft?
                  (for [xi (range minX x)] (list xi y))
                  (for [xi (range (inc x) (inc maxX))] (list xi y)))
        crossings (count-crossings trenchPts sidePts)]
    (odd? crossings)))

(defn inside? [vertWalls pt]
  (let [[x y] pt
        ; arbitrarily chose to look above to decide if it's a vertical wall and not just a corner or horizontal edge
        leftCrossings (filter #(< % x) (get vertWalls y))
        numCrossings (count leftCrossings)]
    (odd? numCrossings)))

;; (inside? (keys (parse-input fname)) 0 6 '(0 0))
;; (inside? (keys (parse-input fname)) 0 6 '(1 1))
;; (inside? (keys (parse-input fname)) 0 6 '(1 3))

(defn part1 []
  (let [{:keys [digs trench extents
                trenchPtsByRow vertWalls]} (parse-input fname)
        [minX minY maxX maxY] extents
        lagoonPts (for [x (range minX (inc maxX))
                        y (range minY (inc maxY))
                        :let [pt [x y]]
                        :when (or (contains? trench pt)
                                  (inside? vertWalls pt))]
                    pt)]
    #_(extents trenchPts)
    (count lagoonPts)))

;(println (part1))


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn parse-line2 [l]
  (let [[_ _ colourStr] (str/split l #" ")
        dirInt (parse-int (subs colourStr 7 8))
        dir (get {3 :up, 0 :right, 1 :down, 2 :left} dirInt)
        len (parse-int (subs colourStr 2 7) 16)]
    (list dir len)))
;(parse-line2 "R 6 (#70c710)")

(defn parse-input2 [fname]
  (->> (slurp fname)
       (str/split-lines)
       (mapv parse-line2)))
;(parse-input2 fname)

(def vertex-type {[:right :up] :RU, [:right :down] :RD,
                  [:up :right] :UR, [:down :right] :DR,
                  [:left  :up] :LU, [:left  :down] :LD,
                  [:up  :left] :UL, [:down  :left] :DL})

(defn delta-x [[[x0 y0] t0] [[x1 y1] t1]]
  {:pre [(= y0 y1),
         (contains? #{:UR :DR :DL :UL} t0)
         (contains? #{:RU :RD :LU :LD} t1)]}
  (+ (- x1 x0)
     (case t0
       :UR 0  ; inclusive and already included, forwards
       :DR -1 ; exclusive and already included, forwards
       :DL 0  ; inclusive and already included, backwards
       :UL 1  ; exclusive and already included, backwards
       (:RU :RD :LU :LD) 0) ; vertical
     (case t1
       :RU 0  ; exclusive and not already included, forwards
       :RD 1  ; inclusive and not already included, forwards
       :LU -1 ; inclusive and not already included, backwards
       :LD 0  ; exclusive and not already included, backwards
       (:UR :DR :DL :UL) 0))) ; vertical

(defn y-height [[[_ y0] t0] [[_ y1] _]]
  {:pre [(= y0 y1), (contains? #{:DL :UL :DR :UR} t0)]}
   (if (contains? #{:DL :UL} t0) (inc y0) y0))

(defn make-vertices [originPt digs]
  (let [firstDig (first digs)
        digsWrapped (conj digs firstDig)
        f (fn [[startPt vertices] [[dir0 len0] [dir1 _]]]
            (let [vertex (move startPt dir0 len0)]
              (list vertex
                    (conj vertices (list
                                    (move startPt dir0 len0)
                                    (get-or-throw vertex-type
                                                  [dir0 dir1]))))))
        [endPt vertices] (reduce f (list originPt [])
                                 (partition 2 1 digsWrapped))]
    (if (= originPt endPt)
      (into [] (cons  (last vertices) (butlast vertices)))
      (throw (Exception. "Digs did not form a closed path.")))))
;; (make-vertices '(0 0) [(list :right 4) (list :down 3) (list :right 5) (list :up 3)
;;                        (list :right 1) (list :down 8) (list :left 10) (list :up 8)])

(defn calculate-area [vertices]
  (let [f (fn [sum [v0 v1]]
            ;(println sum v0 v1)
            (+ sum (* (delta-x v0 v1)
                      (y-height v0 v1))))]
    (abs (reduce f 0 (partition 2 vertices)))))

(defn part2 []
  (let [digs (parse-input2 fname)
        vertices (make-vertices '(0 0) digs)]
    (calculate-area vertices)))


(part2)
