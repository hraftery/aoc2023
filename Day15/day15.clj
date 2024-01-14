(require '[clojure.string :as str])
;(require '[clojure.set :as s])
;(require '[clojure.math.numeric-tower :as m])
;(require ['clojure.math.combinatorics :refer :all])


;;;;;;;;
; Util
;;;;;;;;

(defn sum [seq] (reduce + seq))
(defn prod [seq] (reduce * seq))
(defn parse-int-throws [s] (Integer/parseInt s))
(defn parse-int [s] (try (parse-int-throws s)
                         (catch Exception _ nil)))
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


(defn parse-input [fname]
  (str/split (str/trim (slurp fname)) #","))

(defn calc-hash [s]
  (let [f (fn [acc c]
            (-> (int c)
                (+ acc)
                (* 17)
                (mod 256)))]
  (reduce f 0 s)))
(calc-hash "HASH")

(defn part1 []
  (->> (parse-input fname)
       (map calc-hash)
       sum))

;(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn parse-input2 [fname]
  (let [f (fn [l] (let [[lbl len] (str/split l #"-|=")]
                    { :label lbl :focalLen (parse-int len) }))]
    (map f (parse-input fname))))

(defn do-hashmap [acc step]
  (let [{:keys [label focalLen]} step
        hash (calc-hash label)
        box (get acc hash '())
        match-lens (fn [lens] (= label (lens :label)))]
    ;(println acc step)
    (if focalLen
      (assoc acc hash (list-replace-with box match-lens step))
      (assoc acc hash (filter (complement match-lens) box)))))

(defn draw-boxes [input]
  (let [boxkey2str (fn [boxKey] (str "Box " boxKey ": "))
        lens2str (fn [lens] (str "[" (lens :label) " " (lens :focalLen) "]"))
        box2str (fn [box] (apply str (boxkey2str (key box))
                                     (str/join (map lens2str (val box)))))]
  (println (str/join "\n" (map box2str input)))))

(defn score-box [box]
  (let [lens (enumerate-1-based (map #(% :focalLen) (val box)))]
    (sum (map #(* (inc (key box)) (first %) (second %)) lens))))

(defn score-boxes [boxes]
  (reduce #(+ %1 (score-box %2)) 0 boxes))
(score-boxes {0 '({:label rn, :focalLen 1} {:label cm, :focalLen 2}),
              1 '(),
              3 '({:label ot, :focalLen 7} {:label ab, :focalLen 5} {:label pc, :focalLen 6})})

(defn part2 []
  (->> (parse-input2 fname)
       (reduce do-hashmap {})
       score-boxes))


(part2)
