(require '[clojure.string :as str])
(require '[clojure.algo.generic.functor :refer [fmap]])

; Argh. Why does every modern language (other than Julia) suck at array
; programming? After a few hours, I give up trying to install this and
; and going to DIY it. How hard can it be?
;(require '[clojure.core.matrix :as m])

(defn col-count [m] (count (get m 0)))
(defn row-count [m] (count m))
(defn mget [m x y] (get (get m y) x))


;(def ^:const fname "example.txt")
;(def ^:const fname "example2.txt")
(def ^:const fname "input.txt")

(defn sum [seq] (reduce + seq))
(defn prod [seq] (reduce * seq))
(defn parse-int [s] (Integer/parseInt s))
(defn mapmap [f m] (map (fn [mm] (map f mm)) m))


(def ^:const digits '(\0 \1 \2 \3 \4 \5 \6 \7 \8 \9))
(defn symbol? [c]
  (not (some #{c} (conj digits \.))))

(def ^:const m
  ; very awkwardly add an extra column of periods so that
  ; numbers at the end of a row get terminated correctly
  (into [] (map #(str % \.) (str/split-lines (slurp fname)))))
;; (def ^:const m
;;    (str/split-lines (slurp fname)))
(def ^:const nRows (row-count m))
(def ^:const nCols (col-count m))
(def ^:const allCoords (for [y (range nRows) x (range nCols)] [x y]))

(defn get-neighbours-coords [x y]
  (let [minX (if (> x 0) (- x 1) x)
        maxX (if (< x (- nCols 1)) (+ x 1) x)
        minY (if (> y 0) (- y 1) y)
        maxY (if (< y (- nRows 1)) (+ y 1) y)]
    (for [xi (range minX (+ maxX 1))
          yi (range minY (+ maxY 1))
          :when (not (and (= x xi) (= y yi)))]
      [xi yi])))
(get-neighbours-coords 1 0)

(defn get-neighbours [x y]
  (map #(apply mget m %) (get-neighbours-coords x y)))
(get-neighbours 1 0)

(defn do-extract-numbers [acc [x y]]
  (let [c (mget m x y)
        d (Character/digit c 10)
        s (acc :start)]
    (if (= d -1) ; not a digit
      (if s ; then finish it
        (assoc acc :nums (conj (acc :nums)
                               {:val (acc :val) :row y :start s :end x})
                   :start false
                   :val 0)
        acc) ; not a digit and nothing in progress, so no change
      (if s ; is a digit and we're already started, then just accumulate
        (assoc acc :val (+ d (* 10 (acc :val))))
        ; is a digit and we haven't started, so start
        (assoc acc :val d
                   :start x)))))

(defn extract-numbers []
  (let [numStruct (reduce do-extract-numbers
                          {:nums [] :start false :val 0}
                          allCoords)]
    (numStruct :nums)))

(defn part-number? [num]
  (let [coords (for [x (range (num :start) (num :end))]
                 [x (num :row)])]
    (some identity (for [coord coords]
          (some symbol? (apply get-neighbours coord))))))

(defn extract-part-numbers []
  (filter part-number? (extract-numbers)))

; Part 1
(sum (map #(% :val) (extract-part-numbers)))


; Part 2
(defn extract-gear-coords []
  (filter #(= (apply mget m %) \*) allCoords))

(defn neighbour-of-num? [num coord]
  (let [numCoords (for [x (range (num :start) (num :end))]
                    [x (num :row)])]
    (some identity (for [numCoord numCoords]
                     (some #{coord} (apply get-neighbours-coords numCoord))))))

(defn extract-gear-number-pairs []
  (let [nums (extract-numbers)
        gearCoords (extract-gear-coords)
        numsForEachGear (for [gearCoord gearCoords]
                          (filter #(neighbour-of-num? % gearCoord) nums))]
    (filter #(= 2 (count %)) numsForEachGear)))

(->> (extract-gear-number-pairs)
     (mapmap #(% :val))
     (map #(prod %))
     sum)


