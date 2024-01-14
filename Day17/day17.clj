(require '[clojure.string :as str])
;(require '[clojure.set :as s])
;(require '[clojure.math.numeric-tower :as m])
;(require ['clojure.math.combinatorics :refer :all])

(load-file "./dijkstra.clj")


;;;;;;;;
; Util
;;;;;;;;

(defn sum [seq] (reduce + seq))
(defn prod [seq] (reduce * seq))
(defn parse-int-throws [s] (Integer/parseInt s))
(defn parse-int [s] (try (parse-int-throws s)
                         (catch Exception _ nil)))
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


;;;;;;;;;;
; Part 1
;;;;;;;;;;

;(def ^:const fname "example.txt")
(def ^:const fname "input.txt")


; Produce a city that has blocks, that have a pt and a loss.
(defn parse-input [fname]
  (let [m (str/split-lines (slurp fname))]
    {:blocks (into {} (for [[y row] (enumerate m)
                            [x cell] (enumerate row)]
                        [(list x y) (char2int cell)]))
     :numRows (row-count m) ; iterate *again* to get number of rows
     :numCols (col-count m)})) ; iterate one more time to get number of cols!

(defn in-city? [city pt]
  (let [[x y] pt
        {:keys [numRows numCols]} city]
    (and (>= x 0) (>= y 0) (< x numCols) (< y numRows))))

(defn neighbours [city pt]
  (let [[x y] pt]
    (filter (partial in-city? city)
            [(list (dec x) y) (list x (dec y))
             (list (inc x) y) (list x (inc y))])))
;(neighbours (parse-input fname) '(0 0))

(defn neighbours-with-dir [city pt]
  (let [[x y] pt]
    (filter #(in-city? city (first %))
            [(list (list (dec x) y) :left)
             (list (list x (dec y)) :up)
             (list (list (inc x) y) :right)
             (list (list x (inc y)) :down)])))
;(neighbours-with-dir (parse-input fname) '(1 1))

(defn make-graph [city]
  (let [f (fn [block]
            (let [[pt _] block
                  ns (neighbours city pt)
                  nsMap (select-keys (:blocks city) ns)]
              [pt nsMap]))]
    (into {} (map f (:blocks city)))))

(defn augment-node-with-history [graph pt]
  (let [[x y] pt
        nodeVal (get graph pt)
        no-reverse  (fn [dir orig]
                      (case dir
                        :up    (dissoc orig (list x (inc y)))
                        :right (dissoc orig (list (dec x) y))
                        :down  (dissoc orig (list x (dec y)))
                        :left  (dissoc orig (list (inc x) y))))
        no-straight (fn [dir orig]
                      (case dir
                        :down  (dissoc orig (list x (inc y)))
                        :left  (dissoc orig (list (dec x) y))
                        :up    (dissoc orig (list x (dec y)))
                        :right (dissoc orig (list (inc x) y))))]
    (for [dir [:up :right :down :left]
          straightLineLength (range 1 4)]
      [(list pt dir straightLineLength)
       (if (= straightLineLength 3)
         (no-straight dir (no-reverse dir nodeVal))
         (no-reverse dir nodeVal))])))
;(augment-node-with-history (make-graph (parse-input fname))
;                           '(0 0))

(defn augment-graph-with-history [graph]
  (->> graph
       (map #(augment-node-with-history graph (first %)))
       (apply concat)
       (into {})))
;(augment-graph-with-history (make-graph (parse-input fname)))

(defn opposite-dir [dir]
  (case dir :up :down, :right :left, :down :up, :left :right))

(defn make-node-with-history [city pt]
  (let [blocks (:blocks city)
        ns (neighbours-with-dir city pt)
        remove-dir (fn [dir pts]
                     (remove #(= dir (second %)) pts))]
    (->>
     (for [dir [:up :right :down :left]
           straightLineLength (range 1 4)
           :let [oppDir (opposite-dir dir)
                 nsTrimmed (if (= straightLineLength 3)
                             (remove-dir oppDir (remove-dir dir ns))
                             (remove-dir oppDir ns))
                 nsWithLen (map #(list (first %)
                                       (second %)
                                       (if (= dir (second %))
                                         (inc straightLineLength)
                                         1))
                                nsTrimmed)
                 nsWithCost (map #(vector % (get blocks (first %)))
                                 nsWithLen)]]
      [(list pt dir straightLineLength)
       (into {} nsWithCost)])
     (into {}))))
;(make-node-with-history (parse-input fname)
;                        '(0 0))

(defn make-graph-with-history [city]
  (->> (keys (:blocks city))
       (map (partial make-node-with-history city))
       (apply merge)))
;(make-graph-with-history (parse-input fname))

(defn add-start-node-to-graph [graph] 
  (let [originNodeVal (get graph (list '(0 0) :right 1))
        rightKey (find-first-key #(= '(1 0) (first %)) originNodeVal)
        downKey (find-first-key #(= '(0 1) (first %)) originNodeVal)
        rightLoss (get originNodeVal rightKey)
        downLoss (get originNodeVal downKey)]
    (assoc graph
           (list '(0 0) nil 1) {(list '(1 0) :right 2) rightLoss
                                (list '(0 1) :down 2) downLoss})))

(defn part1 []
  (let [city (parse-input fname)
        graph (->> city
                   make-graph-with-history
                   add-start-node-to-graph)
        {:keys [numRows numCols]} city
        endPt (list (dec numCols) (dec numRows))]
    (for [startNodes [(list '(0 0) nil 1)]
          endDirs [:down :right]
          endLens [1 2 3]
          :let [sols (dijkstra startNodes graph)
                endNodes (list endPt endDirs endLens)]]
      (cost-to endNodes sols))))

;(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn make-node-with-history2 [city pt]
  (let [blocks (:blocks city)
        ns (neighbours-with-dir city pt)
        remove-dir (fn [dir pts]
                     (remove #(= dir (second %)) pts))
        filter-dir (fn [dir pts]
                      (filter #(= dir (second %)) pts))]
    (->>
     (for [dir [:up :right :down :left]
           straightLineLength (range 1 11)
           :let [oppDir (opposite-dir dir)
                 nsTrimmed (cond
                             (= straightLineLength 10)
                               (remove-dir oppDir (remove-dir dir ns))
                             (<= 1 straightLineLength 3)
                               (filter-dir dir ns)
                             :else
                               (remove-dir oppDir ns))
                 nsWithLen (map #(list (first %)
                                       (second %)
                                       (if (= dir (second %))
                                         (inc straightLineLength)
                                         1))
                                nsTrimmed)
                 nsWithCost (map #(vector % (get blocks (first %)))
                                 nsWithLen)]]
       [(list pt dir straightLineLength)
        (into {} nsWithCost)])
     (into {}))))

(defn make-graph-with-history2 [city]
  (->> (keys (:blocks city))
       (map (partial make-node-with-history2 city))
       (apply merge)))

(defn add-start-node-to-graph2 [graph]
  (let [originNodeVal (get graph (list '(0 0) :right 4))
        rightKey (find-first-key #(= '(1 0) (first %)) originNodeVal)
        downKey (find-first-key #(= '(0 1) (first %)) originNodeVal)
        rightLoss (get originNodeVal rightKey)
        downLoss (get originNodeVal downKey)]
    (assoc graph
           (list '(0 0) nil 0) {(list '(1 0) :right 1) rightLoss
                                (list '(0 1) :down 1) downLoss})))

(defn part2 []
  (let [city (parse-input fname)
        graph (->> city
                   make-graph-with-history2
                   add-start-node-to-graph2)
        {:keys [numRows numCols]} city
        endPt (list (dec numCols) (dec numRows))]
    ;(println (into {} (filter #(= '(0 0) (first (key %))) graph)))
    (for [startNodes [(list '(0 0) nil 0)]
          endDirs [:down :right]
          endLens (range 4 11)
          :let [sols (dijkstra startNodes graph)
                endNodes (list endPt endDirs endLens)]]
      (cost-to endNodes sols))))


(println (part2))
