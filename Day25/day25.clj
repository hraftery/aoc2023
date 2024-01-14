(ns day25)

(require '[clojure.string :as str])
(require '[clojure.set :as s])
;(require '[clojure.math.numeric-tower :as m])
;(require '[clojure.math.combinatorics :refer :all])

;(require '[clojure.core.matrix :as m]) ; not supported by Babashka because clojure.reflect is not exposed! OMG, there's goes half a day.
;(require '[clojure.core.matrix.linear :as ml])
;(m/set-current-implementation :vectorz)

(require '[clojure.data.json :as json])


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

(defn parse-line [l]
  (let [[srcStr dstsStr] (str/split l #": ")
        dstStrs (str/split dstsStr #" ")]
    [srcStr (set dstStrs)]))

(defn parse-input [fname]
  (->> (slurp fname)
       (str/split-lines)
       (map parse-line)
       (into {})))
;(parse-input fname)

(defn make-undirected [graph]
  (let [f (fn [k acc x] (update acc x #(if (nil? %)
                                         (set (list k))
                                         (conj % k))))
        g (fn [acc [k v]] (reduce (partial f k) acc v))
        otherDirGraph (reduce g {} graph)]
    (merge-with s/union graph otherDirGraph)))
(make-undirected {"a" (set '("b" "c")), "b" (set '("c" "d"))})

(defn edges [graph]
  (let [f (fn [acc [k v]]
            (concat acc
                    (for [dst v] (list k dst))))
        g (fn [[src dst]] (> (compare src dst) 0))]
  (filterv g (reduce f [] graph))))

(defn connected-nodes
  ([graph startNode] (connected-nodes graph startNode (set nil)))
  ([graph node visitedNodes]
   (let [f (fn [acc x]
             ;(println "step:   " acc x)
             (if (contains? acc x)
               acc
               (connected-nodes graph x acc)))]
     ;(println "reduce: " visitedNodes node (get graph node))
     (reduce f (conj visitedNodes node) (get graph node)))))
;(connected-nodes {1 '(2 3), 2 '(1 3 4 5 6)} 1)

(defn connected-components-counts [graph]
  (loop [remainingNodes (set (keys graph)), counts '()]
    (let [connectedNodes (connected-nodes graph (first remainingNodes))
          counts' (conj counts (count connectedNodes))
          remainingNodes' (s/difference remainingNodes connectedNodes)]
      (if (empty? remainingNodes')
        counts'
        (recur remainingNodes' counts')))))
;(connected-components-counts {1 '(2 3), 2 '(1 3 4 5 6)})
;(connected-components-counts {1 '(0 2), 3 '(4)})

(defn remove-edges [graph edges]
  (let [f (fn [acc edge]
            (-> acc
                (update (first edge) #(disj % (second edge)))
                (update (second edge) #(disj % (first edge)))))]
    (reduce f graph edges)))
(remove-edges (make-undirected {1 (set '(2 3)), 2 (set '(1 3 4 5 6))})
              (list '(2 3)))


(defn part1 [fname]
  (let [g (make-undirected (parse-input fname))
        es (edges g)
        edgeTriples (for [i1 (range 0  (- (count es) 2))
                          i2 (range i1 (- (count es) 1))
                          i3 (range i2 (- (count es) 0))]
                      (list (get es i1) (get es i2) (get es i3)))
        f (fn [edges]
            (->> (remove-edges g edges)
                 connected-components-counts
                 count))
        #_#_#_#_winningTriples (filter #(= (f %) 2) edgeTriples)
        winningCounts (map #(connected-components-counts (remove-edges g %))
                           winningTriples)
        nodes (map (fn [x] {:id x}) (keys g))
        links (map (fn [[src dst]] {:source src, :target dst, :value 1}) es)
        edgeTriple (list (list "pzv" "xft")
                         (list "hbr" "sds")
                         (list "dqf" "cbx"))]
    #_(json/write-str {:nodes nodes, :links links})
    #_(zip winningTriples winningCounts)
    (connected-components-counts (remove-edges g edgeTriple))))

(println (part1 "input.txt"))


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn part2 [fname]
  (let [hailstones (parse-input fname)]
    hailstones))

(part2 "example.txt")

;; (defn main []
;;   (println (part2 "input.txt")))