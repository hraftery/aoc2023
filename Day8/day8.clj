(require '[clojure.string :as str])
;(require '[clojure.set :as s])
(require '[clojure.math.numeric-tower :as m])

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


;;;;;;;;;;
; Part 1
;;;;;;;;;;

;(def ^:const fname "example.txt")
;(def ^:const fname "example2.txt")
(def ^:const fname "input.txt")


(defn parse-node [nodeStr]
  [(subs nodeStr 0 3)
   (list (subs nodeStr 7 10) (subs nodeStr 12 15))])

(defn parse-input [fname infiniteInstructions?]
  (let [[instrStr & nodeStrs] (->> (slurp fname)
                                   str/split-lines)
        instructions (seq instrStr)
        nodesList (map parse-node (rest nodeStrs))
        nodes (into {} nodesList)]
    [(if infiniteInstructions?
       (flatten (repeat (seq instructions)))
       instructions)
     nodes]))
(defn parse-input1 [fname] (parse-input fname true))
(parse-input1 fname)
(def instructions (first (parse-input1 fname)))
(def nodes (second (parse-input1 fname)))

(defn follow-instruction [nodes acc instruction]
  (let [currentNodeName (last acc)
        currentNodeOptions (get nodes currentNodeName)
        nextNode (if (= instruction \L)
                   (first currentNodeOptions)
                   (second currentNodeOptions))]
    ;(println [currentNode currentNodeOptions nextNode])
    (if (= currentNodeName "ZZZ")
      (reduced acc)
      (conj acc nextNode))))
(follow-instruction nodes ["AAA"] \R)
(follow-instruction nodes ["AAA" "CCC"] \L)
(follow-instruction nodes ["AAA" "CCC" "ZZZ"] \L)

(defn follow-instructions [instructions nodes]
  (reduce (partial follow-instruction nodes) ["AAA"] instructions)) ; the number of steps is one less than the number of nodes
(follow-instructions instructions nodes)

(defn part1 []
  (let [[instructions nodes] (parse-input1 fname)]
    (->> (follow-instructions instructions nodes)
         count
         dec))) ; number of steps is one less than the number of nodes

(part1)


;;;;;;;;;;
; Part 2
;;;;;;;;;;

(defn follow-instruction2 [nodes currentNodeName instruction]
  (let [currentNodeOptions (get nodes currentNodeName)]
    (if (= instruction \L)
      (first currentNodeOptions)
      (second currentNodeOptions))))
(follow-instruction2 nodes "AAA" \L)

(defn follow-instructions2 [nodes instructions startingNode]
  (reduce (partial follow-instruction2 nodes)
          startingNode
          instructions))
(follow-instructions2 nodes '(\R \L) "AAA")

(defn part2-bf []
  (let [[instructions nodes] (parse-input1 fname)
        is-starting-node (fn [sym] (= (get (name sym) 2) \A))
        is-finishing-node (fn [sym] (= (get (name sym) 2) \Z))
        startingNodes (filter is-starting-node (map key nodes))]
    (loop [i 1N
           currentNodes startingNodes
           [instruction & nextInstructions] instructions]
      (let [;instruction (first instructions)
            nextNodes (doall ; don't leave lazy seq's clogging up the stack
                       (map #(follow-instruction2 nodes % instruction)
                            currentNodes))]
        ;(println nextNodes)
        (if (zero? (mod i 10000000N))
          (println (unchecked-divide-int i 1000000N))
          nil)
        (if (every? is-finishing-node nextNodes)
          i
          (recur (inc' i) nextNodes nextInstructions))))))

(defn parse-input2 [fname] (parse-input fname false))
(def instructions2 (first (parse-input2 fname)))
(def nodes2 (second (parse-input2 fname)))

(defn is-starting-node [sym] (= (get (name sym) 2) \A))
(defn is-finishing-node [sym] (= (get (name sym) 2) \Z))

(defn follow-instruction2-keep-path [nodes acc instruction]
  (conj acc (follow-instruction2 nodes (last acc) instruction)))

(defn follow-instructions-show-path [nodes instructions startingNode]
  (reduce (partial follow-instruction2-keep-path nodes)
           [startingNode]
           instructions))

(defn follow-instructions-every-node [instructions nodes]
  (let [nodeNames (keys nodes)
        paths (map (partial follow-instructions-show-path nodes instructions)
                   nodeNames)
        dests (map last paths)
        finishingSteps (map (partial positions is-finishing-node)
                            paths)]
    ;(zipmap nodeNames (map list dests finishingSteps))
    ; miraculously, there are no finishing steps during the paths, only at the end points.
    (zipmap nodeNames dests)))
(follow-instructions-every-node instructions2 nodes2)

(defn get-cycle [mappings startingNodeName]
  (loop [acc [], nodeName startingNodeName]
    (let [prevIdx (index-of nodeName acc)]
      (if prevIdx
        {:path acc, :leaderLen prevIdx, :pathLen (count acc)
         :finishingSteps (positions is-finishing-node acc)}
        (recur (conj acc nodeName) (get mappings nodeName))))))
(get-cycle (follow-instructions-every-node instructions2 nodes2) "QNA")

(defn part2 []
  (let [[instructions nodes] (parse-input2 fname)
        startingNodes (filter is-starting-node (map key nodes))
        allInstructionsMapping (follow-instructions-every-node instructions nodes)
        cycles (map (partial get-cycle allInstructionsMapping) startingNodes)]
    ; double miraculously, leaderLen is always 1 and there's always just a
    ; single finishingStep one step before the end. So we need only care about pathLen
    (->> cycles
         (map #(get % :pathLen))
         (map dec)
         (reduce m/lcm)
         (* (count instructions)))
    ))

(println (part2))
