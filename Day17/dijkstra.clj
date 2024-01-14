;
; From https://rosettagit.org/drafts/dijkstras-algorithm/#clojure
;

; Priority Queue
(require '[clojure.data.priority-map :refer [priority-map priority-map-keyfn]])

; Modification of Djikstra: https://www.ummels.de/2014/06/08/dijkstra-in-clojure/
(defn map-vals [m f]
  (into {} (for [[k v] m]
             [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn min2 [v1 v2]
  (if (< (first v1) (first v2))
    v1
    v2))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (graph n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.

  Returns a map from nodes to their distance from start."
  [start graph]
  (loop [q (priority-map-keyfn first start [0 nil]),
         r {}]
    (if-let [[v [d u]] (peek q)]
      (let [dist (-> (graph v)
                     (remove-keys r)
                     (map-vals (fn [cost] (let [new-cost (+ d cost)] [new-cost v]))))]
        (recur (merge-with min2 (pop q) dist) (assoc r v [d u])))
      r)))

(defn make-adj-list
  "Convert a list of nodes and a list of edges into an
  adjacency list structure.  For example: with N [1 2 3],
  E [[1 2] [2 3] [1 3]], the result is {1 [2 3], 2 [3], 3 []}"
  [Nodes Edges]
  (let [init-graph (reduce merge (map #(hash-map %1 {}) Nodes))]
    (reduce #(merge-with merge %1 %2)
            init-graph
            (map #(hash-map (nth % 0) (hash-map (nth % 1) (nth % 2))) Edges))))

(defn path-to [goal dik]
  (if (contains? dik goal)
    (reverse (take-while identity (iterate (comp second dik) goal)))
    nil))

(defn cost-to [goal dik]
  (if (contains? dik goal)
    (first (dik goal))
    -1))
