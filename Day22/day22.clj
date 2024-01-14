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

(defn parse-coord [s]
  (let [[x y z] (map parse-int-throws (str/split s #","))]
    {:x x, :y y, :z z}))

(defn parse-line [l]
  (map parse-coord (str/split l #"~")))
;(parse-line "1,0,1~1,2,1")

(defn parse-input [fname]
  (->> (slurp fname)
       (str/split-lines)
       (mapv parse-line)))
;(parse-input fname)

(defn find-height [bricks]
  (let [f (fn [acc b] (max acc ((first b) :z) ((second b) :z)))]
    (reduce f 0 bricks)))

(defn brick-to-points [brick]
  (let [[p0 p1] brick
        [p0x' p0y' p0z'] [(p0 :x) (p0 :y) (p0 :z)]
        [p1x' p1y' p1z'] [(p1 :x) (p1 :y) (p1 :z)]
        [p0x p1x] (sort [p0x' p1x'])
        [p0y p1y] (sort [p0y' p1y'])
        [p0z p1z] (sort [p0z' p1z'])]
    (cond
      (not= p0x p1x) (for [x (range p0x (inc p1x))] {:x x, :y p0y, :z p0z})
      (not= p0y p1y) (for [y (range p0y (inc p1y))] {:x p0x, :y y, :z p0z})
      (not= p0z p1z) (for [z (range p0z (inc p1z))] {:x p0x, :y p0y, :z z})
      :else (list {:x p0x, :y p0y, :z p0z}))))
  
(defn bricks-to-points [bricks]
  (mapv brick-to-points bricks))

(defn by-layer [brickPts numLayers]
  (let [f (fn [bi b] (list bi b))
        bs (->> (enumerate brickPts)
                (map #(map (partial f (first %)) (second %)))
                (apply concat))]
    (->> (for [z (range 1 (inc numLayers))]
           (for [[bi b] (filter #(= ((second %) :z) z) bs)]
             [(list (b :x) (b :y)) bi]))
         (map (partial into {}))
         (#(conj % {})))))

(defn brick-bottom [ctx brickIdx]
  ((first (nth (ctx :bricks) brickIdx)) :z))

(defn brick-top [ctx brickIdx]
  ((second (nth (ctx :bricks) brickIdx)) :z))

(defn brick-cross-section [ctx brickIdx]
  (set (for [pt (nth (ctx :brickPts) brickIdx)]
         (list (pt :x) (pt :y)))))

(defn drop-brick [ctx brickIdx]
  (let [{:keys [bricks numBricks height brickPts layers]} ctx
        bottom (brick-bottom ctx brickIdx)
        crossSection (brick-cross-section ctx brickIdx)
        potentialFall (range (dec bottom) 0 -1)
        f (fn [layerIdx]
            (->> (set (keys (nth layers layerIdx)))
                 (s/intersection crossSection)
                 count
                 zero?))
        fall (count (take-while f potentialFall))
        reduceZ (fn [dz pt] (update pt :z #(- % dz)))]
    ;(println ctx)
    ;(println bottom crossSection potentialFall fall)
    (if (zero? fall)
      ctx
      (-> ctx
          (update-in [:bricks brickIdx] #(map (partial reduceZ fall) %))
          (update-in [:brickPts brickIdx] #(map (partial reduceZ fall) %))
          (#(assoc % :layers (by-layer (% :brickPts) height)))))))

(defn drop-bricks [ctx]
  (let [{:keys [bricks numBricks height brickPts layers]} ctx
        order (->> (enumerate bricks)
                   (sort-by #((first (second %)) :z))
                   (map first))
        ctx' (reduce drop-brick ctx order)
        height' (find-height (ctx' :bricks))
        layers' (take (inc height') (ctx' :layers))]
    (assoc ctx' :height height' :layers layers')))

(defn critical-bricks [ctx]
  (let [{:keys [bricks numBricks height brickPts layers]} ctx
        f (fn [acc brickIdx]
            (let [bottom (brick-bottom ctx brickIdx)]
              (if (= bottom 1)
                acc
                (let [cs (brick-cross-section ctx brickIdx)
                      belowLayer (nth layers (dec bottom))
                      belowXYs (set (keys belowLayer))
                      supportingXYs (s/intersection cs belowXYs)
                      supportingBrickIdx (belowLayer (first supportingXYs))]
                  (if (every? #(= supportingBrickIdx (belowLayer %))
                              supportingXYs)
                    (conj acc supportingBrickIdx)
                    acc)))))]
    (reduce f (set nil) (range numBricks))))

(defn part1 []
  (let [bricks (parse-input fname)
        numBricks (count bricks)
        height (find-height bricks)
        brickPts (bricks-to-points bricks)
        layers (by-layer brickPts height)
        ctx {:bricks bricks, :numBricks numBricks, :height height,
             :brickPts brickPts, :layers layers}]
    ;(println ctx)
    ;(println (drop-bricks ctx))
    (->> (drop-bricks ctx)
         critical-bricks
         count
         (- numBricks))))

;(println (part1))


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn supported-bricks [ctx]
  (let [{:keys [bricks numBricks height brickPts layers]} ctx
        f (fn [brickIdx]
            (let [top (brick-top ctx brickIdx)]
              (if (= top height)
                (set nil)
                (let [cs (brick-cross-section ctx brickIdx)
                      aboveLayer (nth layers (inc top))
                      aboveXYs (set (keys aboveLayer))
                      supportingXYs (s/intersection cs aboveXYs)]
              (set (map aboveLayer supportingXYs))))))]
    (mapv f (range numBricks))))

(defn supporting-bricks [ctx]
  (let [{:keys [bricks numBricks height brickPts layers]} ctx
        f (fn [brickIdx]
            (let [bottom (brick-bottom ctx brickIdx)]
              (if (= bottom 1)
                (set nil)
                (let [cs (brick-cross-section ctx brickIdx)
                      belowLayer (nth layers (dec bottom))
                      belowXYs (set (keys belowLayer))
                      supportingXYs (s/intersection cs belowXYs)]
                  (set (map belowLayer supportingXYs))))))]
    (mapv f (range numBricks))))

(defn will-fall [supporting supported brickIdx]
  (loop [checklist (get supported brickIdx)
         fallen (set (list brickIdx))]
    (let [g (fn [brickIdx] (s/difference (get supporting brickIdx)
                                         fallen))
          newFallen (filter #(empty? (g %)) checklist)]
      ;(println checklist fallen newFallen)
      (if (empty? newFallen)
        (disj fallen brickIdx)
        (let [fallen' (apply conj fallen newFallen)
              newChecklistList (map (partial get supported) newFallen)
              checklist' (apply s/union newChecklistList)]
          (recur checklist' fallen'))))))

(defn part2 []
  (let [bricks (parse-input fname)
        numBricks (count bricks)
        height (find-height bricks)
        brickPts (bricks-to-points bricks)
        layers (by-layer brickPts height)
        ctx {:bricks bricks, :numBricks numBricks, :height height,
             :brickPts brickPts, :layers layers}
        ctx' (drop-bricks ctx)
        critical (critical-bricks ctx')
        supporting (supporting-bricks ctx')
        supported (supported-bricks ctx')]
    ;(println ctx)
    ;(println ctx')
    ;(println critical)
    ;(println supported)
    ;(println supporting)
    #_(will-fall supporting supported 0)
    (->> critical
         (map #(count (will-fall supporting supported %)))
         sum)))

(println (part2))
