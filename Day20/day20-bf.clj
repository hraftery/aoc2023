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

(defn parse-line [l]
  (let [[srcStr dstsStr] (str/split l #" -> ")
        dstStrs (str/split dstsStr #", ")
        [srcName srcType] (case (first srcStr)
                            \% [(subs srcStr 1) :f]
                            \& [(subs srcStr 1) :c]
                            [srcStr :b])]
    [srcName {:type srcType :dsts dstStrs}]))

(defn parse-input [fname]
  (->> (slurp fname)
       (str/split-lines)
       (map parse-line)
       (into {})))
;(parse-input fname)

(defn find-sources [modConf name]
  (filter #(seq-contains? ((get modConf %) :dsts) name)
          (keys modConf)))

(defn initialise-configuration [modConf]
  (fmapkv (fn [k v]
            (case (:type v)
              :b v
              :f (assoc v :state :off)
              :c (assoc v :state
                        (into {}
                              (for [src (find-sources modConf k)]
                                [src :low])))))
        modConf))

(defn apply-pulse [modConf [level src dst]]
  (if (not (contains? modConf dst)) ; "output" in the examples, "rx" in the input
    [modConf '()]
    (let [{:keys [type dsts]} (get modConf dst)]
      (case type
        :f (if (= level :high)
             [modConf '()]
             (let [state (get-in modConf [dst :state])
                   state' ({:off :on, :on :off} state)
                   level' ({:off :low, :on :high} state')
                   modConf' (assoc-in modConf [dst :state] state')
                   newPulses (for [dst' dsts] [level' dst dst'])]
               [modConf' newPulses]))
        :c (let [modConf' (assoc-in modConf [dst :state src] level)
                 level' (if (every? #(= :high %)
                                    (vals (get-in modConf' [dst :state])))
                          :low
                          :high)
                 newPulses (for [dst' dsts] [level' dst dst'])]
             [modConf' newPulses])))))

(defn press-button [modConf]
  (let [initialPulses (for [dst ((modConf "broadcaster") :dsts)]
                        [:low "broadcaster" dst])]
    (loop [modConf modConf, pulses (queue initialPulses),
           pulseCount {:low (inc (count pulses)), :high 0}]
      ;(println "loop: " modConf)
      ;(println "loop: " pulses)
      ;(println pulseCount)
      ;(println (peek pulses))
      (if (empty? pulses)
        [modConf pulseCount]
        (let [[modConf' newPulses] (apply-pulse modConf (peek pulses))
              pulses' (apply conj (pop pulses) newPulses)
              {:keys [low high]} pulseCount
              newLow  (count (filter #(= (first %) :low)  newPulses))
              newHigh (count (filter #(= (first %) :high) newPulses))
              pulseCount' {:low  (+ low newLow) :high (+ high newHigh)}]
          ;(println pulseCount)
          (recur modConf' pulses' pulseCount'))))))

(defn part1 []
  (loop [i 0, pulseCountLow 0, pulseCountHigh 0,
         modConf (initialise-configuration (parse-input fname))]
    (let [[modConf' pulseCount] (press-button modConf)
          i' (inc i)
          pulseCountLow'  (+ pulseCountLow  (get pulseCount :low))
          pulseCountHigh' (+ pulseCountHigh (get pulseCount :high))]
      (if (= i' 1000)
        [pulseCountLow' pulseCountHigh']
        (recur i' pulseCountLow' pulseCountHigh' modConf')))))

;(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn press-button2 [modConf]
  (let [initialPulses (for [dst ((modConf "broadcaster") :dsts)]
                        [:low "broadcaster" dst])]
    (loop [modConf modConf, pulses (queue initialPulses),
           pulseCount {:low (inc (count pulses)), :high 0},
           lowRxPulseCount 0]
      ;(println "loop: " modConf)
      ;(println "loop: " pulses)
      ;(println pulseCount)
      ;(println (peek pulses))
      (if (empty? pulses)
        [modConf pulseCount lowRxPulseCount]
        (let [[modConf' newPulses] (apply-pulse modConf (peek pulses))
              pulses' (apply conj (pop pulses) newPulses)
              {:keys [low high]} pulseCount
              newLow  (count (filter #(= (first %) :low)  newPulses))
              newHigh (count (filter #(= (first %) :high) newPulses))
              pulseCount' {:low  (+ low newLow) :high (+ high newHigh)}
              newLowRxPulseCount (count (filter #(and (= (first %) :low)
                                                      (= (last %) "rx")) newPulses))
              lowRxPulseCount' (+ lowRxPulseCount newLowRxPulseCount)]
          ;(println pulseCount)
          (recur modConf' pulses' pulseCount' lowRxPulseCount'))))))

(defn part2 []
  (loop [i 0, pulseCountLow 0, pulseCountHigh 0,
         modConf (initialise-configuration (parse-input fname))]
    (let [[modConf' pulseCount lowRxPulseCount] (press-button2 modConf)
          i' (inc i)
          pulseCountLow'  (+ pulseCountLow  (get pulseCount :low))
          pulseCountHigh' (+ pulseCountHigh (get pulseCount :high))]
      (if (= (mod i' 1000000) 0) (println i' lowRxPulseCount))
      (if (= lowRxPulseCount 1)
        i'
        (recur i' pulseCountLow' pulseCountHigh' modConf')))))

(println (part2))
