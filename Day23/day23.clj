(ns day23)

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

(defn parse-input [fname]
  (let [m (str/split-lines (slurp fname))
        cells (into {}
                    (for [[y row] (enumerate m)
                          [x cell] (enumerate row)]
                      [(list x y) cell]))
        numRows (row-count m)
        numCols (col-count m)]
    {:forest (set (keys (filter #(= (second %) \#) cells)))
     :paths (set (keys (filter #(= (second %) \.) cells)))
     :slopes (into {} (filter #(seq-contains? '(\^ \> \v \<) (second %)) cells))
     :numRows numRows
     :numCols numCols
     :startPt '(1 0)
     :endPt (list (- numCols 2) (- numRows 1))}))
;(parse-input fname)

(defn neighbours [trails pt]
  (let [{:keys [paths slopes]} trails
        [x y] pt
        path?  (fn [p] (contains? paths p))
        slope? (fn ([p] (contains? slopes p))
                   ([p slope] (= (get slopes p) slope)))
        N (list x (dec y)), E (list (inc x) y)
        S (list x (inc y)), W (list (dec x) y)
        N? (if (slope? pt) (slope? pt \^) (or (path? N) (slope? N \^)))
        E? (if (slope? pt) (slope? pt \>) (or (path? E) (slope? E \>)))
        S? (if (slope? pt) (slope? pt \v) (or (path? S) (slope? S \v)))
        W? (if (slope? pt) (slope? pt \<) (or (path? W) (slope? W \<)))]
    (reduce (fn [acc [include p]] (if include (conj acc p) acc))
            '() (list [N? N] [E? E] [S? S] [W? W]))))
;; (neighbours (parse-input fname) '(9 3))
;; (neighbours (parse-input fname) '(11 3))
;; (neighbours (parse-input fname) '(10 3))

(defn neighbours-except [trails excludePt pt]
  (filter #(not= excludePt %) (neighbours trails pt)))

(defn children [trails grandparentPt parentPt]
  (let [{:keys [paths slopes endPt]} trails
        pathOrSlope? (fn [p] (or (contains? paths p)
                                 (contains? slopes p)))
        f (fn [pt]
            (loop [prevPt parentPt, pt pt, dist 1]
              (let [[x y] pt
                    N (list x (dec y)), E (list (inc x) y)
                    S (list x (inc y)), W (list (dec x) y)
                    nextPts (filter #(and (not= prevPt %)
                                          (pathOrSlope? %))
                                    (list N E S W))]
                (case (count nextPts)
                  0 [pt (if (= pt endPt) dist -1)] ; deadend
                  1 (recur pt (first nextPts) (inc dist))
                  [pt dist]))))]
  (map f (neighbours-except trails grandparentPt parentPt))))

(defn make-graph [trails]
  (let [{:keys [paths slopes]} trails
        pathsAndSlopes (apply conj paths (keys slopes))
        f (fn [pt]
            (let [ns (neighbours trails pt)
                  nsMap (into {} (zip ns (repeat -1)))]
              [pt nsMap]))]
    (into {} (map f pathsAndSlopes))))

; Dijkstra can't do longest path. Floyd–Warshall would probably do, just by negating the weights,
; but is O(n^3). Bellman-Ford can do it with the same negative weight trick, and is only O(n*m).
; A topological sort + critical path will also do and is "textbook". But given our particular
; graph, a DFS with Dynamic Programming seems both intuitive and efficient.
(defn find-longest-path
  ([trails]
   (let [{:keys [startPt endPt]} trails]
     (-> (find-longest-path trails nil startPt 0 {endPt 0})
         (get startPt))))
  ([trails parentPt pt dist longestPaths]
   ;(println parentPt pt dist longestPaths)
   (if (contains? longestPaths pt)
     longestPaths
     (let [children (children trails parentPt pt)
           f (fn [acc [childPt dist]]
               (find-longest-path trails pt childPt dist acc))
           longestPaths' (reduce f longestPaths children)
           longestPath (apply max (map #(+ (second %) (get longestPaths' (first %))) children))]
       ;(println parentPt pt longestPaths children longestPaths' longestPath)
       (if (or (empty? children) (= longestPath -1))
         (assoc longestPaths pt -1)
         (assoc longestPaths' pt longestPath))))))

;; (defn draw-cost-map [trails sols]
;;   (let [{:keys [forest numRows numCols]} trails
;;         do-col (fn [y] (map #(if (contains? forest (list % y)) "##"
;;                                  (format "%02d" (abs (cost-to (list % y) sols))))
;;                             (range numCols)))]
;;     (map #(apply str (do-col %)) (range numRows))))

(defn part1 [fname]
  (let [trails (parse-input fname)]
    ;(println trails)
    (find-longest-path trails)))

;(part1 "input.txt")


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn slopes2paths [trails]
  (let [{:keys [paths slopes]} trails
         pathsAndSlopes (apply conj paths (keys slopes))]
     (assoc trails :paths pathsAndSlopes
                   :slopes {})))

(defn neighbours-except2 [trails excludePts pt]
  (remove (partial contains? excludePts) (neighbours trails pt)))

(defn children2 [trails grandparentPts parentPt]
  (let [{:keys [paths endPt]} trails
        path? (fn [p] (contains? paths p))
        f (fn [pt]
            (loop [prevPts (conj grandparentPts parentPt), pt pt, dist 1]
              (let [[x y] pt
                    N (list x (dec y)), E (list (inc x) y)
                    S (list x (inc y)), W (list (dec x) y)
                    nextPts (filter #(and (path? %)
                                          (not (contains? prevPts %)))
                                    (list N E S W))]
                (case (count nextPts)
                  0 [pt (if (= pt endPt) dist nil)] ; deadend
                  1 (recur (conj prevPts pt) (first nextPts) (inc dist))
                  [pt dist]))))]
    (->> (neighbours-except2 trails grandparentPts parentPt)
         (map f)
         (filter #(some? (second %))))))

(defn find-longest-path2
  ([trails] (find-longest-path2 trails (set nil) (trails :startPt)))
  ([trails parentPts pt]
   ;(println "out: " parentPts pt)
   (let [children (children2 trails parentPts pt)
         parentPts' (conj parentPts pt)
         f (fn [acc [childPt distToChild]]
             (let [distToEnd (if (= childPt (trails :endPt))
                               0
                               (find-longest-path2 trails parentPts' childPt))]
               (if (nil? distToEnd)
                 acc
                 ((fnil max 0) acc (+ distToChild distToEnd)))))
         mapf (fn [[childPt distToChild]]
                (let [distToEnd (if (= childPt (trails :endPt))
                                  0
                                  (find-longest-path2 trails parentPts' childPt))]
                  (if (nil? distToEnd)
                    nil
                    (+ distToChild distToEnd))))
         reducef (fn [acc x]
                   (if (nil? x)
                     acc
                     ((fnil max 0) acc x)))]
     ;(println "in:  " parentPts pt children)
     (if (empty? children)
       nil
       (reduce f nil children)
       #_(reduce reducef nil (pmap mapf children))))))

(defn part2 [fname]
  (let [trails (slopes2paths (parse-input fname))]
    (find-longest-path2 trails)))

;(part2 "example.txt")

(defn main []
  (println (part2 "input.txt")))
