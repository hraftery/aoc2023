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
(defn firsts [coll] ; like Haskell's "tails": (firsts [1 2 3 4]) => ([1] [1 2] [1 2 3] [1 2 3 4])
  (rest (reductions (fn [acc x] (conj acc x)) [] coll)))


;;;;;;;;;;
; Part 1
;;;;;;;;;;

;(def ^:const fname "example.txt")
(def ^:const fname "input.txt")

(defn parse-condition [s]
  (if (nil? s)
    (fn [part] true)
    (let [category (get s 0)
          lessThan (= (get s 1) \<)
          value (parse-int-throws (subs s 2))]
      (fn [part]
        (let [{:keys [x m a s]} part
              catValue (case category \x x, \m m, \a a, \s s)]
          (if lessThan (< catValue value)
              (> catValue value)))))))
;; ((parse-condition nil) {:x 100, :m 100, :a 100, :s 100})
;; ((parse-condition "x<10") {:x 9, :m 99, :a 99, :s 99})
;; ((parse-condition "x<10") {:x 10, :m 1, :a 1, :s 1})

(defn parse-rule [s]
  (let [[condPart destPart] (str/split s #":")]
    (if destPart
      (list (parse-condition condPart) destPart)
      (list (parse-condition nil) condPart))))
;; (let [[f dest] (parse-rule "m<20:two")]
;;   [dest (f {:x 20, :m 19, :a 20, :s 20})])

(defn parse-workflow [l]
  (let [[_ name rulesStr] (re-matches #"(.*?)\{(.*?)\}" l)]
    [name (mapv parse-rule (str/split rulesStr #","))]))
;(parse-workflow "ex{x>10:one,m<20:two,a>30:R,A}")

(defn parse-part-rating [l]
  (let [[_ x m a s] (re-matches #"\{x=(.*?),m=(.*?),a=(.*?),s=(.*?)\}" l)]
    {:x (parse-int-throws x), :m (parse-int-throws m),
     :a (parse-int-throws a), :s (parse-int-throws s)}))
;(parse-part-rating "{x=787,m=2655,a=1222,s=2876}")

(defn parse-input [fname]
  (let [[workflowLines _ partRatingLines] (->> (slurp fname)
                                             (str/split-lines)
                                             (partition-by str/blank?))
        workflows (into {} (mapv parse-workflow workflowLines))
        partRatings (mapv parse-part-rating partRatingLines)]
    [workflows partRatings]))
;(parse-input fname)

(defn apply-workflow [workflows name part]
  (let [rules (get-or-throw workflows name)]
    (reduce (fn [_ [condition destination]]
            (when (condition part) (reduced destination)))
          nil rules)))
;; (let [workflows (into {} (mapv parse-workflow ["ex{x>10:one,m<20:two,a>30:R,A}"]))]
;;   (apply-workflow workflows "ex" {:x 10, :m 20, :a 30, :s 2876}))

(defn process-part [workflows part]
  (loop [wfName "in"]
    (let [newWfName (apply-workflow workflows wfName part)]
      ;(println "workflow result: " newWfName)
      (if (list-contains? '("R" "A") newWfName)
        newWfName
        (recur newWfName)))))


(defn part1 []
  (let [[workflows partRatings] (parse-input fname)]
    (->> partRatings
         ;(map (partial process-part workflows))
         (filter #(= "A" (process-part workflows %)))
         (map #(sum (vals %)))
         sum)))

;(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn parse-condition2 [s]
  (if (nil? s)
    nil
    (let [category (keyword (str (get s 0)))
          lessThan (= (get s 1) \<)
          value (parse-int-throws (subs s 2))]
      (list category lessThan value))))
;(parse-condition2 nil)
;(parse-condition2 "x<10")

(defn parse-rule2 [s]
  (let [[condPart destPart] (str/split s #":")]
    (if destPart
      (list (parse-condition2 condPart) destPart)
      (list (parse-condition2 nil) condPart))))
;(parse-rule2 "m<20:two")

(defn parse-workflow2 [l]
  (let [[_ name rulesStr] (re-matches #"(.*?)\{(.*?)\}" l)]
    [name (mapv parse-rule2 (str/split rulesStr #","))]))
;(parse-workflow "ex{x>10:one,m<20:two,a>30:R,A}")

(defn parse-input2 [fname]
  (let [[workflowLines _ _] (->> (slurp fname)
                                 (str/split-lines)
                                 (partition-by str/blank?))
        workflows (into {} (mapv parse-workflow2 workflowLines))]
    workflows))

(defn apply-rule [condition validPart]
  (if (nil? condition)
    validPart
    (let [[category lessThan value] condition
          f (fn [[lower upper]]
              (if lessThan (list lower (min value upper))
                           (list (max value lower) upper)))]
      (update validPart category f))))
;(apply-rule '(:a true 2006) {:x '(0 4001), :m '(0 4001), :a '(0 4001), :s '(0 4001)})

(defn apply-rule-inverse [condition validPart]
  {:pre [condition]}
  (let [[category lessThan value] condition
        newValue (if lessThan (dec value)
                              (inc value))]
    (apply-rule (list category (not lessThan) newValue)
                validPart)))

(defn process-workflow [workflows name validPart]
  (case name
    "R" nil
    "A" validPart
    (let [rules (get workflows name)
          f (fn [rules]
              (let [[currCond destination] (last rules)
                    prevConds (map first (butlast rules))
                    validPart' (reduce #(apply-rule-inverse %2 %1)
                                       validPart prevConds)
                    validPart'' (apply-rule currCond validPart')]
                ;(println "Curr: " [currCond destination] " Prev:" prevConds)
                ;(println "validPart: " validPart " --> " validPart' " --> " validPart'')
                (process-workflow workflows destination validPart'')))]
      ;(println name)
      (flatten (remove nil? (map f (firsts rules)))))))

(defn calculate-valid-ratings [validParts]
  (let [f (fn [validPart]
            (->> (vals validPart)
                 (map (fn [[lower upper]] (dec (- upper lower))))
                 prod))]
    (sum (map f validParts))))

(defn part2 []
  (let [workflows (parse-input2 fname)]
    (->> {:x '(0 4001), :m '(0 4001), :a '(0 4001), :s '(0 4001)}
         (process-workflow workflows "in")
         calculate-valid-ratings)))


(part2)
