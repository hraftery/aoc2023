(require '[clojure.string :as str])
(require '[clojure.set :as s])
(require '[clojure.math :as m])

;(def ^:const fname "example.txt")
;(def ^:const fname "example2.txt")
(def ^:const fname "input.txt")

(defn sum [seq] (reduce + seq))
(defn prod [seq] (reduce * seq))
(defn parse-int [s] (Integer/parseInt s))
(defn mapmap [f m] (map (fn [mm] (map f mm)) m))
(defn str2nums [s] (map parse-int (str/split s #"\s+")))

(defn parse-line [l]
  (let [numsStr (first (rest (str/split l #":\s+")))
        numsSplitStr (str/split numsStr #"\s+\|\s+")
        numsSplitNums (map str2nums numsSplitStr)
        winningNums (first numsSplitNums)
        myNums (first (rest numsSplitNums))] ; sheesh, destructuring after a map is hard
  [winningNums myNums]))
(parse-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

(defn parse-line-as-sets [l]
  (map set (parse-line l)))
(parse-line-as-sets "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

(defn count-wins [winningNums myNums]
  (count (s/intersection winningNums myNums)))

(defn score-wins [numWins]
  (if (= 0 numWins)
    0 ; bit-shift-left actually rotates?? so need to handle 0 separately
    (bit-shift-left 1 (dec numWins))))

; Part 1
(->> (slurp fname)
     str/split-lines
     (map parse-line-as-sets)
     (map (partial apply count-wins))
     (map score-wins)
     sum)


; Part 2
(defn add-copies [cardIdx allCardWins allCardCounts]
  (let [wins (get allCardWins cardIdx)
        maxIdx (+ cardIdx wins)
        winningCardCount (aget allCardCounts cardIdx)]
    (loop [i (inc cardIdx)]
      (when (<= i maxIdx)
        (->> (+ (aget allCardCounts i) winningCardCount)
             int
             (aset allCardCounts i))
        (recur (inc i))))))

(def numOrigCards (count (str/split-lines (slurp fname))))
(def allCardCounts (int-array (repeat numOrigCards 1)))

(def cardWins
  (->> (slurp fname)
       str/split-lines
       (map parse-line-as-sets)
       (mapv (partial apply count-wins))))


(loop [i 0]
  (when (< i numOrigCards)
    (add-copies i cardWins allCardCounts)
    (recur (inc i))))

(sum allCardCounts)
