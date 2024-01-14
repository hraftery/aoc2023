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
(defn enumerate [l] (map-indexed list l))
(defn range-from [start] (drop-while #(> start %) (range)))
(defn col-count [m] (count (get m 0)))
(defn row-count [m] (count m))
(defn mget [m x y] (get (get m y) x))


;;;;;;;;;;
; Part 1
;;;;;;;;;;

;(def ^:const fname "example.txt")
;(def ^:const fname "example2.txt")
;(def ^:const fname "example24.txt")
(def ^:const fname "input.txt")

(defn parse-input [fname]
  (let [m (str/split-lines (slurp fname))
        cells (into {}
                    (for [[y row] (enumerate m)
                          [x cell] (enumerate row)]
                      (if (= cell \S)
                        [cell (list x y)] ; make it easier to find later
                        [(list x y) cell])))
        startPt (cells \S)] ; seems nuts I have to search twice just to note which one is \S
    {:startPt startPt
     :cells (-> cells
                (dissoc \S) ; undo the "if = cell \S" hack above
                (assoc startPt \S))
     :numRows (row-count m) ; iterate *again* to get number of rows
     :numCols (col-count m)}))  ; iterate one more time to get number of cols!
(parse-input fname)

(defn determine-s-pipe [input]
  (let [{:keys [startPt cells]} input
        [x y] startPt
        isUp    (some #{(cells (list x (dec y)))} '(\| \7 \F))
        isDown  (some #{(cells (list x (inc y)))} '(\| \J \L))
        isLeft  (some #{(cells (list (dec x) y))} '(\- \L \F))
        isRight (some #{(cells (list (inc x) y))} '(\- \7 \J))]
    (cond
      (and isUp isDown)    \|
      (and isUp isLeft)    \J
      (and isUp isRight)   \L
      (and isDown isLeft)  \7
      (and isDown isRight) \F
      (and isLeft isRight) \-)))
(determine-s-pipe {:startPt '(1 1)
                   :cells {'(1 0) \L, '(1 2) \|, '(0 1) \7, '(2 1) \-}})

(defn exits [pt cells]
  (let [[x y] pt
        up    (list x (dec y))
        down  (list x (inc y))
        left  (list (dec x) y)
        right (list (inc x) y)]
    (case (cells pt)
      \| [up down]
      \- [left right]
      \L [up right]
      \J [up left]
      \7 [down left]
      \F [down right])))

(defn find-loop [input]
  (let [{:keys [startPt cells]} input]
    (loop [loopPts [], currPt startPt, prevPt nil]
      (let [nextLoopPts (conj loopPts currPt)
            [exit1 exit2] (exits currPt cells)
            nextPt (if (= exit1 prevPt) exit2 exit1)]
        ;(println [loopPts currPt prevPt exit1 exit2 nextPt])
        (if (= nextPt startPt)
          nextLoopPts
          (recur nextLoopPts nextPt currPt))))))


(defn part1 []
  (let [{:keys [startPt cells] :as origInput} (parse-input fname)
        sPipe (determine-s-pipe origInput)
        newCells (assoc cells startPt sPipe)
        input (assoc origInput :cells newCells)
        loopPts (find-loop input)]
    (/ (count loopPts) 2)))

;(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn count-crossings [cells loopPts ptsToCount]
  (let [is-crossing-pipe (fn [pipe] (some #{pipe} '(\| \F \7)))
        is-crossing (fn [pt] (and (some #{pt} loopPts)
                                  (is-crossing-pipe (cells pt))))]
    (count (filter is-crossing ptsToCount))))


(defn is-inside [ctx aPt]
  (let [{loopPts :loopPts, cells :cells, cols :numCols} ctx
        [x y] aPt
        leftPts (for [xi (range 0 x)] (list xi y))
        rightPts (for [xi (range (inc x) cols)] (list xi y))
        leftCrossings (count-crossings cells loopPts leftPts)
        rightCrossings (count-crossings cells loopPts rightPts)]
    (if (odd? (+ leftCrossings rightCrossings))
      (throw (Exception. "Must be even number of crossings."))
      (odd? leftCrossings))))


(defn part2 []
  (let [{startPt :startPt, origCells :cells, :as origInput} (parse-input fname)
        sPipe (determine-s-pipe origInput)
        cells (assoc origCells startPt sPipe)
        input (assoc origInput :cells cells)
        loopPts (find-loop input)
        ctx (assoc input :loopPts loopPts)] ; bundle everything into a "context" to pass around
    (->> cells
         keys
         (filter #(not (some #{%} loopPts)))
         (filter (partial is-inside ctx))
         count)))

(part2)
