(require '[clojure.string :as str])
(require '[clojure.algo.generic.functor :refer [fmap]])

;(def ^:const fname "example.txt")
;(def ^:const fname "example2.txt")
(def ^:const fname "input.txt")

(defn sum [seq] (reduce + seq))
(defn prod [seq] (reduce * seq))
(defn parse-int [s] (Integer/parseInt s))
(defn mapmap [f m] (map (fn [mm] (map f mm)) m))
(defn mapfirst [f m]
  (map (fn [mm] #(vector (map f (first mm))
                         (second mm)) m)))


; Part 1
(defn parse-cubes [s]
  (->> (str/split s #", ")
       (map #(str/split % #" "))
       (map #(vector (second %) (parse-int (first %))))
       (into {})))
(parse-cubes "1 red, 2 green, 6 blue")

(defn parse-line [s]
  (-> s
      (str/split #"Game |: |; ")
      rest
      (as-> fields (vector (parse-int (first fields))
                           (map parse-cubes (rest fields))))))
(parse-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

(defn valid-game? [m]
  (and (<= (get m "red" 0) 12)
       (<= (get m "green" 0) 13)
       (<= (get m "blue" 0) 14)))


(->> (slurp fname)
     str/split-lines
     (map parse-line)
     (into {}) ;; complete dataset
     (fmap (partial apply merge-with max))
     ; careful, filter turns the map of mpas into a "lazy sequence of map entries" but that's okay by me
     ; https://stackoverflow.com/questions/2753874/how-to-filter-a-persistent-map-in-clojure
     (filter #(valid-game? (val %)))
     keys
     sum
     )


; Part 2
(->> (slurp fname)
     str/split-lines
     (map parse-line)
     (into {}) ;; complete dataset
     (fmap (partial apply merge-with max))
     (fmap (comp prod vals))
     vals
     sum)

