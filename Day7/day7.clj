(require '[clojure.string :as str])
;(require '[clojure.set :as s])
;(require '[clojure.math :as m])

;;;;;;;;
; Util
;;;;;;;;

(defn sum [seq] (reduce + seq))
(defn prod [seq] (reduce * seq))
(defn parse-int [s] (Integer/parseInt s))
(defn is-digit [c] (Character/isDigit c))
(defn mapmap [f m] (map (fn [mm] (map f mm)) m))
(defn str2nums [s] (map read-string (str/split s #"\s+")))
(defn str-filter [pred s] (apply str (filter pred s)))
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
; Part 1
;;;;;;;;;;

;(def ^:const fname "example.txt")
;(def ^:const fname "example2.txt")
(def ^:const fname "input.txt")

(defn parse-hand [handStr]
              ; sort turns the map of character-freq pairs into a list of array pairs, but at least we get lowest freq first
  (let [groups (sort-by val (frequencies handStr))
        numGroups (count groups)
        highestFreq (second (last groups))
        type (cond
               (= numGroups 1)          7 ; five of a kind
               (and (= numGroups 2)
                    (= highestFreq 4))  6 ; four of a kind
               (and (= numGroups 2)
                    (= highestFreq 3))  5 ; full house
               (and (= numGroups 3)
                    (= highestFreq 3))  4 ; three of a kind
               (and (= numGroups 3)
                    (= highestFreq 2))  3 ; two pair
               (= numGroups 4)          2 ; one pair
               (= numGroups 5)          1 ; high card
               )
        card2strength (fn [c] (case c
                                \A 14
                                \K 13
                                \Q 12
                                \J 11
                                \T 10
                                (- (int c) (int \0))))]
    ;(println [groups numGroups highestFreq])
    ;(conj type (map card2strength handStr))))
    (->> type
         (conj (map card2strength handStr)) ; note map makes a list, so this will *prepend* type
         (into [])))) ; lists don't support compare, so a vector means hands will sort natively
(parse-hand "TTT98")

(defn parse-input [fname parseHandFn]
  (let [f (fn [handStr bidStr] [(parseHandFn handStr)
                                (parse-int bidStr)])]
    (->> (slurp fname)
         str/split-lines
         (map #(str/split % #"\s+"))
         (map (partial apply f)))))


(defn part1 []
  (->> (parse-input fname parse-hand)
       sort
       (map-indexed (fn [i x] (* (inc i) (second x))))
       sum))

(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn apply-jokers [groups]
  (if (= (count groups) 1)
    groups ; don't bother if only 1 group. Avoid drama when that group is J!
    (let [jFreq (get groups \J 0) ; default of 0 means no effect
          groupsNoJ (dissoc groups \J) ; has no effect if no J's
          bestCard (key (apply max-key val groupsNoJ))]
      (update groupsNoJ
              bestCard
              (partial + jFreq)))))
(apply-jokers (frequencies "KTJJT"))
(apply-jokers (frequencies "JJJJJ"))

(defn parse-hand2 [handStr]
  (let [groups (sort-by val (apply-jokers (frequencies handStr)))
        numGroups (count groups)
        highestFreq (second (last groups))
        type (cond
               (= numGroups 1)          7 ; five of a kind
               (and (= numGroups 2)
                    (= highestFreq 4))  6 ; four of a kind
               (and (= numGroups 2)
                    (= highestFreq 3))  5 ; full house
               (and (= numGroups 3)
                    (= highestFreq 3))  4 ; three of a kind
               (and (= numGroups 3)
                    (= highestFreq 2))  3 ; two pair
               (= numGroups 4)          2 ; one pair
               (= numGroups 5)          1 ; high card
               )
        card2strength (fn [c] (case c
                                \A 14
                                \K 13
                                \Q 12
                                \J 0
                                \T 10
                                (- (int c) (int \0))))]
    ;(println [groups numGroups highestFreq])
    ;(conj type (map card2strength handStr))))
    (->> type
         (conj (map card2strength handStr)) ; note map makes a list, so this will *prepend* type
         (into [])))) ; lists don't support compare, so a vector means hands will sort natively
(parse-hand2 "TTJ98")

(defn part2 []
  (->> (parse-input fname parse-hand2)
       sort
       (map-indexed (fn [i x] (* (inc i) (second x))))
       sum))

(println (part2))
