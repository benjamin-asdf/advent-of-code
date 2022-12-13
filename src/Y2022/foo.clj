(ns Y2022.foo)



















(require '[clojure.set :as set])


;; This function takes a starting node and a graph as its arguments. The graph is assumed to be represented as an adjacency list, where each node is mapped to a sequence of its adjacent nodes.

;; The paths-from function uses a recursive loop to build all the paths that start at the given starting node. It keeps track of the visited? nodes in the visited? set, and the resulting paths in the acc accumulator.

;; In each iteration of the loop, the function gets the adjacent nodes of the current node (the last node in the first path in the acc accumulator) using the adjacency list. Then, it constructs new paths by appending each unvisited adjacent node to the current path. Finally, it updates the visited? set and the acc accumulator with the new paths, and continues the loop until there are no more adjacent nodes to visit.

;; Once all the paths have been built, the paths-from function returns the resulting acc accumulator, which contains all the paths that start at the given starting node.

(defn paths-from [start graph]
  (loop [visited? #{start}
         acc [[start]]]
    (let [adj-list (graph (peek (first acc)))]
      (if (seq adj-list)
        (let [new-paths (for [node adj-list
                              :let [new-path (conj (first acc) node)]
                              :when (not (visited? node))]
                          new-path)]
          (if (seq new-paths)
            (recur (set/union visited? (set (map peek new-paths)))
                   (concat acc new-paths))
            acc))
        acc))))

(defn all-paths [graph start end]
  (let [paths (paths-from start graph)]
    (for [path paths
          :when (last path) == end]
      path)))


(paths-from 1 {1 [2 3] 2 [4]})

(paths-from 1 {1 [2 3] 2 [4] 3 [2]})

(defn paths-from [start graph]
  (loop [visited #{start}
         acc [[start]]
         adj-list (graph start)]
    (if-not (seq adj-list)
      acc
      (let [new-paths (for [node adj-list
                            :let [new-path (conj (first acc) node)]
                            :when (not (visited node))]
                        new-path)]
        (if-not (seq new-paths)
          acc
          (recur (set/union visited (set (map last new-paths)))
                 (concat acc new-paths)
                 (graph (last (first acc)))))))))

(->>
 (paths-from
  1
  {1 [2 3]
   2 [5]
   5 [4]
   3 [1 4]
   4 [3]}))

(defn paths-from [start graph]
  (loop [visited #{start}
         acc [[start]]
         adj-list (graph start)]
    (if-not (seq adj-list)
      acc
      (let [new-paths (for [node adj-list
                            :let [new-path (conj (last acc) node)]
                            :when (not (visited node))]
                        new-path)]
        (if-not (seq new-paths)
          acc
          (recur (set/union visited (set (map last new-paths)))
                 (concat acc new-paths)
                 (graph (last (first new-paths)))))))))
