;; not completed.
;; BFS should be the most promising
;; the most promising is shortest-path but somethign is wrong

(ns
    Y2022.day12
    (:require
     [clojure.string :as str]
     [clojure.set :as set]
     [clojure.core.matrix :as m]))

(def
  input
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn parse [input]
  (->>
   (str/split-lines input)
   (map #(re-seq #"\w" %))
   (mapv vec)
   (m/emap
    (fn [e]
      (assoc
       (into [] (repeat (inc (- (int \z) (int \a))) nil))
       (- (int (first (case e "S" "a" "E" "z" e))) (int \a))
       e)))
   (m/matrix)))

(defn
  neighbors-1
  [[x y z]]
  (into
   []
   cat
   (for
       [z (range 0 (inc (inc z)))]
       [[(inc x) y z]
        [(dec x) y z]
        [x (inc y) z]
        [x (dec y) z]])))


(defn get-neighbors [node m]
  (keep
   #(try (when (apply m/mget m %) %) (catch Throwable _))
   (neighbors-1 node)))

(defn search [start-node goal-node graph]
  (let [f
        (fn [{:keys [current-path paths visited?] :as state}]
          (let [e (peek current-path)]
            (cond
              (not e)
              {:done paths}
              (paths current-path)
              (update state :current-path pop)
              (= e goal-node)
              (-> state
                  (update :paths conj current-path)
                  (update :current-path pop))
              :else
              (let [neighbors
                    (set/difference
                     (set (graph e))
                     (get visited? e)
                     (set current-path))]
                (if-not (seq neighbors)
                  (update state :paths conj current-path)
                  (-> state
                      (update :current-path conj (first neighbors))
                      (update-in [:visited? e] (fnil conj #{}) (first neighbors))))))))]
    (first (keep :done (iterate f {:current-path [start-node] :paths #{} :visited? {}})))))

(->>
 (search
  1
  4
  {1 [2 3]
   2 [5]
   5 [4]
   3 [1 4]
   4 [3]})
 (filter (comp #{4} peek))
 ;; (map count)
 ;; (sort -)
 ;; first
 )

(defn
  find-goal
  [m]
  (m/ereduce
   (fn [acc e] (or acc e))
   (m/emap-indexed
    (fn [i e] (when (= e "E") i))
    m)))

(defn search
  "Performs a graph search starting at the given node and returns the
   path from the start node to the goal node, or nil if no path is
   found.
   The graph is represented as a map of node names to sets of
   neighboring nodes.
   The search algorithm used is breadth-first search."
  [graph start-node goal-node]
  (loop [paths (list [start-node]) visited (set start-node)]
    (if-let [[current-node & path] (first paths)]
      (if (= current-node goal-node)
        paths
        (let [neighbors (set (graph current-node))]
          (recur (concat (map #(concat path [%]) (set/difference neighbors visited)) (rest paths))
                 (set/union visited neighbors))))
      paths)))

(count (let [goal (find-goal matrix)]
         (->>
          (search
           (fn [node] (get-neighbors node matrix))
           [0 0 0]
           goal)
          )))



(defn visited?
  "Predicate which returns true if the node v has been visited already, false otherwise."
  [v coll]
  (some #(= % v) coll))


(defn find-neighbors
  "Returns the sequence of neighbors for the given node"
  [v coll]
  (get coll v))

(def graph {:A [:B :C]
            :B [:A :X]
            :X [:B :Y]
            :Y [:X]
            :C [:A :D]
            :D [:C :E :F]
            :E [:D :G]
            :F [:D :G]
            :G [:E :F]})

(defn graph-dfs
  "Traverses a graph in Depth First Search (DFS)"
  [graph v]
  (loop [stack   (vector v) ;; Use a stack to store nodes we need to explore
         visited []]        ;; A vector to store the sequence of visited nodes
    (if (empty? stack)      ;; Base case - return visited nodes if the stack is empty
      visited
      (let [v           (peek stack)
            neighbors   (graph v)
            not-visited (filter (complement #(visited? % visited)) neighbors)
            new-stack   (into (pop stack) not-visited)]
        (if (visited? v visited)
          (recur new-stack visited)
          (recur new-stack (conj visited v)))))))

(graph-dfs
 (fn [node] (find-neighbors node graph))
 :X)


[:X :Y :B :A :C :D :F :G :E]

(let [graph (fn [node] (get-neighbors node matrix))
      ;; goal  (find-goal matrix)
      ]
  (->>
   (graph-dfs
    graph [0 0 0])
   ;; (take-while (complement #{goal}))
   ;; count
   ;; inc
   )
  )

(defn paths-from [start graph]
  (loop [visited #{start}
         acc [[start]]
         adj-list (graph start)]
    (if-not (seq adj-list)
      acc
      (let [new-paths (for [node adj-list
                            :let [new-path (conj (peek acc) node)]
                            :when (not (visited node))]
                        new-path)]
        (if-not (seq new-paths)
          acc
          (recur (set/union visited (into #{} (map last) new-paths))
                 (into acc new-paths)
                 (into [] (comp (map last) (map graph) cat) new-paths)))))))

(let [goal (find-goal matrix)]
  (->> (paths-from
        [0 0 0]
        (fn [node] (get-neighbors node matrix)))
       (filter
        (comp #{goal} peek))
       (map count)
       (sort -)
       first
       dec
       )

  )
31

(->>
 (paths-from
  1
  {1 [2 3]
   2 [5]
   5 [4]
   3 [1 4]
   4 [3]})
 )
(let [matrix
      (parse (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day12"))
      goal (find-goal matrix)]
  (->> (paths-from
        [0 0 0]
        (fn [node] (get-neighbors node matrix)))
       (filter
        (comp #{goal} peek))
       (map count)
       (sort -)
       first
dec
       )

  )
(defn
  paths-from-2
  [start current-path goal graph]
  (if
      (= goal start)
      [current-path]
      (let [adj-list (graph start)
            adj-list (remove
                      (set current-path)
                      adj-list)]
        (if-not
            (seq adj-list)
            [current-path]
            (sequence
             (mapcat
              (fn
                [node]
                (paths-from-2
                 node
                 (into current-path [node])
                 goal
                 graph)))
             adj-list)))))


(->>
 (paths-from-2
  1
  [1]
  4
  {1 [2 3]
   2 [5]
   5 [4]
   3 [1 4]
   4 [3]})
 )

(->>
 (paths-from-2
  1
  [1]
  4
  {1 [2 3]
   2 [5]
   5 [4]
   3 [1 4]
   4 [3]})
 )
(let [goal (find-goal matrix)]
  (->>
   (paths-from-2
    [0 0 0]
    [[0 0 0]]
    goal
    (fn [node] (get-neighbors node matrix)))
   (filter
    (comp #{goal} peek))
   (map count)
   (sort -)
   first
   dec


   )

  )


(let [matrix
      (parse (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day12"))
      goal (find-goal matrix)
      ]
  (->>
   (paths-from-2
    [0 0 0]
    [[0 0 0]]
    goal
    (fn [node] (get-neighbors node matrix)))
   (filter
    (comp #{goal} peek))
   (map count)
   (sort -)
   first
   dec
   )
  )

(defn shortest-path [start end graph]
  (loop [visited #{start}
         queue [start]
         parents {start nil}]
    (if-not (seq queue)
      []
      (let [node (first queue)
            neighbors (remove visited (graph node))]
        (if (= node end)
          (let [path (loop [node end acc []]
                       (if (nil? node)
                         acc
                         (recur (parents node) (cons node acc))))]
            path)
          (recur (into visited neighbors)
                 (into (rest queue) neighbors)
                 (reduce (fn [m n] (assoc m n node)) parents neighbors)))))))

(->>
 (shortest-path
  1
  4
  {1 [2 3]
   2 [5]
   5 [4]
   3 [1 4]
   4 [3]})
 )


(dec
 (count
  (let [matrix matrix
        ;; (parse (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day12"))
        goal (find-goal matrix)]
    (->>
     (shortest-path
      [0 0 0]
      goal
      (fn [node] (get-neighbors node matrix))))
    ;; (filter
    ;;  (comp #{goal} peek))
    ;; (map count)
    ;; (sort -)
    ;; first
    ;; dec

    )))


(dec
 (count
  (let [matrix
        (parse (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day12"))
        goal (find-goal matrix)]
    (->>
     (shortest-path
      [0 0 0]
      goal
      (fn [node]
        (sort-by #(nth % 2) < (get-neighbors node matrix))))))))

(defn find-highest-neighbor [{:keys [grid current visited?]}]
  (def grid grid)
  (def current current)
  (def visited? visited?)
  {:grid grid
   :visited? (conj visited? current)
   :current
   (first (remove visited? (sort-by #(nth % 2) > (get-neighbors current grid))))})

(defn hill-climbing [graph start goal]
  (->>
   (iterate find-highest-neighbor {:grid graph :current start :visited? #{}})
   (drop-while (comp #(not= % goal) :current))
   count))

(let [matrix
      (parse (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day12"))
      goal (find-goal matrix)]
  (hill-climbing
   matrix [0 0 0]
   goal
   )

  ;; (filter
  ;;  (comp #{goal} peek))
  ;; (map count)
  ;; (sort -)
  ;; first
  ;; dec

  )
