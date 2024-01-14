(require '[clojure.string :as str])


;;;;;;;;
; Util
;;;;;;;;

(defn sum [seq] (reduce + seq))
(defn prod [seq] (reduce * seq))
(defn parse-int [s] (Integer/parseInt s))
(defn mapmap [f m] (map (fn [mm] (map f mm)) m))
(defn str2nums [s] (map read-string (str/split s #"\s+")))
(defn list-insert [lst elem index]
  (let [[l r] (split-at index lst)]
    (concat l [elem] r)))
(defn find-first [f coll]
  (first (filter f coll)))
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


;;;;;;;;;;
; Impl 1
;;;;;;;;;;

(def ^:const fname "input.txt")
;(def ^:const fname "example2.txt")
;(def ^:const fname "input.txt")


(defn parse-seeds [seedsStr]
  (-> seedsStr
      (str/split #": ")
      (get 1)
      (str2nums)))
(parse-seeds "seeds: 79 14 55 13")

(defn nums2range [[t f c]] {:to t :from f :count c})
(nums2range '(50 98 2))

(defn parse-ranges [mapStr]
  (->> mapStr
       (map str2nums)
       (map nums2range)))
(parse-ranges '("50 98 2" "52 50 48"))

(defn parse-input [fname]
  (let [[seedsStrs & mapStrs] (->> (slurp fname)
                                   str/split-lines
                                   (filter seq)
                                   (partition-by #(str/includes? % "map:"))
                                   (take-nth 2))]
    {:seeds (parse-seeds (first seedsStrs))
     :maps (map parse-ranges mapStrs)}))

(defn add-range-to-keyed-map [acc {:keys [to from count]}]
  (let [; everything within this range has the difference between to and from added to it, 
        lower {:from from :add (- to from)}
        ; everything beyond that is unchanged, unless another range overrules
        upper {:from (+ from count) :add 0 }]
    (if (contains? acc (upper :from)) ; only add the upper default if there's not already something set
      (assoc acc (lower :from) (lower :add))
      (assoc acc (lower :from) (lower :add)
                 (upper :from) (upper :add)) ; surely there's a way to not repeat myself here?
      )))
(-> {}
    (add-range-to-keyed-map {:to 0, :from 15, :count 37})
    (add-range-to-keyed-map {:to 39, :from 0, :count 15})
    (add-range-to-keyed-map {:to 37, :from 52, :count 2}))

; Convert from the input format to an ordered map, with an entry for each range
; including the ranges outside those specified. The range's upper bound is the key,
; so all you need to do to find the right range is iterate through until your value
; is no longer below the key.
(defn optimise-map [aMap]
  (->> aMap
       (reduce add-range-to-keyed-map {})
       (into (sorted-map))))
(optimise-map '({:to 0, :from 15, :count 37} {:to 37, :from 52, :count 2} {:to 39, :from 0, :count 15}))

(defn find-map-range-for-num [num aMap]
  (->> (take-while #(>= num (key %)) aMap)
       (last)))

(defn apply-map [num aMap]
  (->> (find-map-range-for-num num aMap)
       (second)
       ((fnil (partial + num) 0))))
(apply-map 10 {45 32, 68 -4, 81 -36, 100 0})
(apply-map 79 {50 2, 98 -48, 100 0})
(apply-map 77 {0 39, 15 -15, 52 -15, 54 0})


(defn part1 []
  (let [{:keys [seeds maps]} (parse-input fname)
        oMaps (map optimise-map maps)]
    ;(println {:seeds seeds :maps oMaps})
    (apply min (map #(reduce apply-map % oMaps) seeds))))

(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn expand-seed-range [seedRange]
  (let [[start len] seedRange]
    (range start (+ start len))))
(expand-seed-range '(79 14))

(defn expand-seed-ranges [seedRanges]
  (->> seedRanges
       (partition 2)
       (map expand-seed-range)
       (flatten)))
(expand-seed-ranges '(79 14 55 13))

; Ah, so the key to part 2 is to *not* expand the ranges. Have to deal with ranges the whole way through.
(defn make-seed-interval [seedRange]
  (let [[start len] seedRange]
    [start (dec (+ start len))]))

(defn make-seed-intervals [seedRanges]
  (->> seedRanges
       (partition 2)
       (map make-seed-interval)
       (sort)))
(make-seed-intervals '(79 14 55 13))

(defn do-consolidate-intervals [{:keys [res curr]} [thisStart thisEnd]]
  (if (seq curr)
    (let [[currStart currEnd] curr
          newEnd (max currEnd thisEnd)]
      (if (>= (inc currEnd) thisStart)
    ; Scenario 1: curr in progress and this to be incorporated
        {:res res :curr [currStart newEnd]}
    ; Scenario 2: curr in progress and this to be separate
        {:res (conj res curr) :curr [thisStart newEnd]}))
    ; Scenario 3: curr not in progress (only happens once at start)
    {:res res :curr [thisStart thisEnd]}))
(do-consolidate-intervals {:res [] :curr []} [40 49])
(do-consolidate-intervals {:res [] :curr [40 49]} [50 50])
(do-consolidate-intervals {:res [] :curr [40 50]} [52 99])
(do-consolidate-intervals {:res [[40 50]] :curr [52 99]} [90 92])
(do-consolidate-intervals {:res [[40 50]] :curr [52 99]} [95 105])
(do-consolidate-intervals {:res [[40 50]] :curr [52 105]} [110 110])
(do-consolidate-intervals {:res [[40 50] [52 105]] :curr [110 110]} [111 112])

(defn consolidate-intervals [seedIntervals]
  (let [{:keys [res curr]} (reduce do-consolidate-intervals
                                   {:res [] :curr []}
                                   seedIntervals)]
    (conj res curr)))
(consolidate-intervals [[40 49] [50 50] [52 99] [90 92] [95 105] [110 110] [111 112]])
(consolidate-intervals (sort '([50 50] [52 99] [40 49])))

(defn find-map-range-with-upper-bound-for-num [num aMap]
  (let [last2 (take-last 2 (take-until #(< num (key %)) aMap))
        f (first last2)
        l (last last2)
        max (dec (key l))]
    (if (= (count last2) 1)
      {:mapRange [num 0] :max max} ; below map ranges
      (if (>= num (key l))
        {:mapRange l :max nil} ; above map ranges
        {:mapRange f :max max}))))  ; within map ranges
(find-map-range-with-upper-bound-for-num 40 {50 2, 98 -48, 100 0})

(defn apply-map-to-interval [interval aMap]
  ; Replace nil with [] to create a vector that grows to the right instead of a
  ; list that grows to the left. I don't have a preference at the moment.
  (loop [interval interval aMap aMap res nil]
    (let [[start end] interval
          {:keys [mapRange max]} (find-map-range-with-upper-bound-for-num start aMap)
          [mapStart mapAdd] mapRange
          withinMapRange (or (nil? max) (<= end max))]
      (if withinMapRange
        (conj res [(+ start mapAdd) (+ end mapAdd)])
        (recur [(inc max) end] aMap (conj res [(+ start mapAdd) (+ max mapAdd)]))))))
(apply-map-to-interval '[40 49] {50 2, 98 -48, 100 0})
(apply-map-to-interval '[50 61] {50 2, 98 -48, 100 0})
(apply-map-to-interval '[101 103] {50 2, 98 -48, 100 0})
(apply-map-to-interval '[40 51] {50 2, 98 -48, 100 0})
(apply-map-to-interval '[40 98] {50 2, 98 -48, 100 0})

(defn apply-map-to-intervals [intervals aMap]
  (mapcat #(apply-map-to-interval % aMap) intervals))
(apply-map-to-intervals [[40 98] [101 103]] {50 2, 98 -48, 100 0})

(defn apply-map-to-intervals-and-consolidate [intervals aMap]
  (->> (apply-map-to-intervals intervals aMap)
       sort
       consolidate-intervals))
(apply-map-to-intervals-and-consolidate '([55 67] [79 92]) {50 2, 98 -48, 100 0})


(defn part2 []
  (let [{:keys [seeds maps]} (parse-input fname)
        oMaps (map optimise-map maps)
        seedIntervals (make-seed-intervals seeds)]
    ;(println {:seeds seedIntervals :maps oMaps})
    (first
     (reduce apply-map-to-intervals-and-consolidate
             seedIntervals
             oMaps))))

(part2)
