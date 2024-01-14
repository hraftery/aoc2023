(ns day24)

(require '[clojure.string :as str])
(require '[clojure.set :as s])
;(require '[clojure.math.numeric-tower :as m])
;(require ['clojure.math.combinatorics :refer :all])
;(require ['clojure.math :as ])
(require '[clojure.core.matrix :as m]) ; not supported by Babashka because clojure.reflect is not exposed! OMG, there's goes half a day.
(require '[clojure.core.matrix.linear :as ml])
(m/set-current-implementation :vectorz)


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

(defn parse-triplet [s]
  (let [[x y z] (map #(read-string (str/trim %)) (str/split s #","))]
    {:x x, :y y, :z z}))

(defn parse-line [l]
  (let [[p v] (map parse-triplet (str/split l #" @ "))]
    {:p p, :v v}))
;(parse-line "19, 13, 30 @ -2,  1, -2")

(defn parse-input [fname]
  (->> (slurp fname)
       (str/split-lines)
       (mapv parse-line)))
;(parse-input "example.txt")

(defn intersection-time-xy [h1 h2]
  (let [[p1x p1y v1x v1y] [((h1 :p) :x) ((h1 :p) :y)
                           ((h1 :v) :x) ((h1 :v) :y)]
        [p2x p2y v2x v2y] [((h2 :p) :x) ((h2 :p) :y)
                           ((h2 :v) :x) ((h2 :v) :y)]
        A (m/matrix [[v1x (- v2x)]
                     [v1y (- v2y)]])
        b [(- p2x p1x) (- p2y p1y)]]
    (ml/solve A b)))

(defn project-xy [h t]
  (let [A (m/matrix [[((h :p) :x) ((h :v) :x)]
                     [((h :p) :y) ((h :v) :y)]])]
    (m/mmul A [1 t])))

(defn valid-intersection-xy? [h1 h2 validLow validHigh]
  (let [[t1 t2] (m/eseq (intersection-time-xy h1 h2))]
    (if (or (nil? t1) (neg? t1) (neg? t2))
      false
      (let [[px py] (m/eseq (project-xy h1 t1))]
        (and (<= validLow px validHigh)
             (<= validLow py validHigh))))))

(defn part1 [fname]
  (let [hailstones (parse-input fname)
        numHailstones (count hailstones)
        pairs (for [i (range (dec numHailstones))
                    j (range (inc i) numHailstones)]
                [(nth hailstones i) (nth hailstones j)])
        f (fn [[h1 h2]]
            (valid-intersection-xy? h1 h2
                                    200000000000000
                                    400000000000000))]
    (count (filter f pairs))))

(part1 "input.txt")
;(part1 "input.txt")


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn decompose [h]
  [((h :p) :x) ((h :p) :y) ((h :p) :z)
   ((h :v) :x) ((h :v) :y) ((h :v) :z)])

(defn compose [px py pz vx vy vz]
  {:p {:x px, :y py, :z pz}
   :v {:x vx, :y vy, :z vz}})

(defn intersecting-line [h1 h2 h3]
  (let [[p1x p1y p1z v1x v1y v1z] (decompose h1)
        [p2x p2y p2z v2x v2y v2z] (decompose h2)
        [p3x p3y p3z v3x v3y v3z] (decompose h3)
        A (m/matrix
           [[(- v1y v2y) (- v2x v1x) 0 (- p2y p1y) (- p1x p2x) 0]
            [(- v1y v3y) (- v3x v1x) 0 (- p3y p1y) (- p1x p3x) 0]
            [(- v1z v2z) 0 (- v2x v1x) (- p2z p1z) 0 (- p1x p2x)]
            [(- v1z v3z) 0 (- v3x v1x) (- p3z p1z) 0 (- p1x p3x)]
            [0 (- v1z v2z) (- v2y v1y) 0 (- p2z p1z) (- p1y p2y)]
            [0 (- v1z v3z) (- v3y v1y) 0 (- p3z p1z) (- p1y p3y)]])
        b [(- (+ (* p1x v1y) (* p2y v2x)) (+ (* p1y v1x) (* p2x v2y)))
           (- (+ (* p1x v1y) (* p3y v3x)) (+ (* p1y v1x) (* p3x v3y)))
           (- (+ (* p1x v1z) (* p2z v2x)) (+ (* p1z v1x) (* p2x v2z)))
           (- (+ (* p1x v1z) (* p3z v3x)) (+ (* p1z v1x) (* p3x v3z)))
           (- (+ (* p1y v1z) (* p2z v2y)) (+ (* p1z v1y) (* p2y v2z)))
           (- (+ (* p1y v1z) (* p3z v3y)) (+ (* p1z v1y) (* p3y v3z)))]]
    (apply compose (m/eseq (ml/solve A b)))))

(defn part2 [fname]
  (let [hailstones (parse-input fname)
        h1 (nth hailstones 0)
        h2 (nth hailstones 1)
        h3 (nth hailstones 2)
        h0 (intersecting-line h1 h2 h3)]
    #_h0
    (Math/round (+ ((h0 :p) :x)
                   ((h0 :p) :y)
                   ((h0 :p) :z)))))

(part2 "input.txt")

;; (defn main []
;;   (println (part2 "input.txt")))