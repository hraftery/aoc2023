(require '[clojure.string :as str])
;(require '[clojure.set :as s])
;(require '[clojure.math.numeric-tower :as m])
;(require ['clojure.math.combinatorics :refer :all])


;;;;;;;;
; Util
;;;;;;;;

(defn sum [seq] (reduce + seq))
(defn prod [seq] (reduce * seq))
(defn parse-int [s] (Integer/parseInt s))
(defn is-digit [c] (Character/isDigit c))
(defn mapmap [f m] (map (fn [mm] (map f mm)) m))
(defn str2nums [s] (map read-string (str/split s #"\s+")))
(defn csv2nums [s] (map parse-int (str/split s #",")))
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


;;;;;;;;;;
; Part 1
;;;;;;;;;;

(def ^:const fname "example.txt")
;(def ^:const fname "input.txt")

(defn parse-line [l]
  (let [[conditionsStr groupsStr] (str/split l #" ")
        unknownLocs (positions (partial = \?) conditionsStr)
        groups (csv2nums groupsStr)]
    {:conditionsStr conditionsStr
     :unknownLocs unknownLocs
     :groups groups}))

(defn parse-input [fname]
  (->> (slurp fname)
       str/split-lines
       (map parse-line)))
;(parse-input fname)

(defn make-conditions-str [row unknowns]
  (let [{:keys [conditionsStr unknownLocs]} row]
    (reduce #(replace-at %1 (first %2) (second %2))
            conditionsStr
            (zipmap unknownLocs unknowns))))
(make-conditions-str {:conditionsStr ".??..??...?##." :unknownLocs '(1 2 5 6 10)}
                     '(\# \. \#))

(defn get-groups [conditionsStr]
  (let [strSoFar (first (str/split conditionsStr #"\?" 2))
        strGroups (str/split strSoFar #"\.+")]
    (drop-while zero? (map count strGroups))))
(get-groups ".##..??...?##.")
(get-groups "???.###")
(get-groups "#.?.###")
(get-groups "#.#.###")
(get-groups "#.#.???.?")
(get-groups "#.#.###.#")
(get-groups "#.#.###.?")
(get-groups "#.#.##?.?")
(get-groups "#.#.#.?.?")


(defn is-valid [conditionsStr groups]
  (let [numUnknown (count (filter #(= \? %) conditionsStr))
        numSprings (count (filter #(= \# %) conditionsStr))
        targetNumSprings (sum groups)
        myGroups (get-groups conditionsStr)
        zippedMaybeEmpty (zip myGroups groups)
        ; make special case of hitting a ? before a # valid (because it's too early to tell) instead of blowing up
        zipped (if (seq zippedMaybeEmpty) zippedMaybeEmpty {0 0})]
    (and ; first make sure we haven't exceeded our target
     (<= numSprings targetNumSprings)
     (<= (count myGroups) (count groups))
         ; then make sure we can get there. ie. has to be enough springs or potential springs to get the job done
     (>= (+ numUnknown numSprings) targetNumSprings)
         ; and every group so far has to match
     (every? #(= (first %) (second %)) (butlast zipped))
         ; except the last one, which can still be growing
     (<= (first (last zipped)) (second (last zipped))))))
(is-valid "#.#.###" '(1,1,3))
(is-valid "???.###" '(1,1,3))
(is-valid "#??.###" '(1,1,3))
(is-valid "##?.###" '(1,1,3))
(is-valid ".#?.###" '(1,1,3))
(is-valid ".#.#.##" '(1,1,3))
(is-valid ".#..###" '(1,1,3))
(is-valid "#.#..??" '(1,1,3))
(is-valid "#.#.???" '(1,1,3))
(is-valid "..?.###" '(1,1,3))
(is-valid ".###.##.#..." '(3,2,1))
(is-valid ".###.##.#.#." '(3,2,1))
(is-valid ".###.#.#.#.." '(3,2,1))
(is-valid "#...#.#.##.#" '(1,1,1,3))

;; (defn count-solutions-bad-recur [row]
;;   (let [{:keys [unknownLocs groups]} row]
;;     (loop [unknowns [], acc []]
;;       (let [myConditionsStr (make-conditions-str row unknowns)]
;;         (println [myConditionsStr groups unknowns unknownLocs])
;;         (if (is-valid myConditionsStr groups)
;;           (if (= (count unknowns) (count unknownLocs))
;;             myConditionsStr ; winner!
;;             (conj acc (recur (conj unknowns \#) acc)
;;                       (recur (conj unknowns \.) acc)))
;;           nil ; abandon this path
;;         )))))

(defn count-solutions
  ([row] (count-solutions row [] []))
  ([row unknowns acc]
   (let [{:keys [unknownLocs groups]} row
         myConditionsStr (make-conditions-str row unknowns)]
     ;(println [myConditionsStr groups unknowns unknownLocs (is-valid myConditionsStr groups) (= (count unknowns) (count unknownLocs))])
     (if (is-valid myConditionsStr groups)
       (if (= (count unknowns) (count unknownLocs))
         myConditionsStr ; winner!
         (conj-unless-nil acc (count-solutions row (conj unknowns \#) acc)
                          (count-solutions row (conj unknowns \.) acc)))
       nil ; abandon this path
       ))))

(defn part1 []
  (->> (parse-input fname)
       (map count-solutions)
       (map flatten)
       (map count)
       sum))

;(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn parse-line2 [l]
  (let [[conditionsStr1 groupsStr1] (str/split l #" ")
        conditionsStr (str/join "?" (repeat 5 conditionsStr1))
        groupsStr (str/join "," (repeat 5 groupsStr1))
        unknownLocs (positions (partial = \?) conditionsStr)
        groups (csv2nums groupsStr)]
    {:conditionsStr conditionsStr
     :unknownLocs unknownLocs
     :groups groups}))

(defn parse-input2 [fname]
  (->> (slurp fname)
       str/split-lines
       (map parse-line2)))
;(parse-input2 fname)

(defn count-solutions2
  ([row] (count-solutions2 row [] [] {}))
  ([row unknowns acc cache]
   (let [{:keys [unknownLocs groups]} row
         myConditionsStr (make-conditions-str row unknowns)
         level (count unknowns)
         incompleteGroup (and (< level (count unknownLocs))
                              (= \#
                                 (get myConditionsStr
                                      (dec (nth unknownLocs level))
                                      \.)))
         state (list (get-groups myConditionsStr)
                     level
                     incompleteGroup)]
     ;(println (last unknowns) " | " state " | " myConditionsStr " |a " acc " |c " cache)
     (if (is-valid myConditionsStr groups)
       (if (= (count unknowns) (count unknownLocs))
         ;(do (println "win" " | " state " | " myConditionsStr) (list myConditionsStr cache)) ; winner!
         (list myConditionsStr cache) ; winner!
         (if (contains? cache state)
           ;(do (println "cache hit! " " | " state " | " (get cache state)) (list (apply conj-unless-nil [] (get cache state)) cache))
           (list (apply conj-unless-nil [] (get cache state)) cache)

           (let [[leftAcc leftCache] (count-solutions2 row (conj unknowns \#) acc cache)
                 [rightAcc rightCache] (count-solutions2 row (conj unknowns \.) acc leftCache)]
             ;(println "branch" " | " state " | " myConditionsStr " |la " leftAcc " |ra " rightAcc " |lc " leftCache " |rc " rightCache " |c " cache)
             (list (conj-unless-nil acc leftAcc rightAcc)
                   (assoc rightCache state [leftAcc rightAcc])))))
                  ;;  (if (seq (first state))
                  ;;    (assoc rightCache state [leftAcc rightAcc])
                  ;;    rightCache)))))
       (do ;(println "stop" " | " state " | " myConditionsStr " | " acc " | " cache)
           (list nil cache)) ; abandon this path
       ))))

(defn part2 []
  (->> (parse-input2 fname)
       last
       count-solutions2
       #_second
       first
       flatten
       count))

(part2)
