(ns Y2022.dijkstra
  )

(import [clojure.lang PersistentQueue IPersistentStack IPersistentCollection])


(require '[clojure.data.priority-map :as pm])

;; add some key-value pairs to the map
(def pmap (assoc pmap :foo 1 :bar 2 :baz 3))

(defn queue [& args]
  (into PersistentQueue/EMPTY args))

(deftype PriorityQueue [priority-map priority]
  IPersistentCollection
  (cons [_ v] (PriorityQueue. (assoc priority-map v (priority v)) priority))
  IPersistentStack
  (peek [_] (first (peek priority-map)))
  (pop [_] (PriorityQueue. (pop priority-map) priority)))

;; create a new priority queue with the priority function
(def queue (PriorityQueue. (pm/priority-map) identity))

(defn nodes [graph] (keys graph))
(def graph-dist (constantly 1))

(pop (into
      (PriorityQueue. (pm/priority-map) identity)
      [1 2 3]))


(peek (pop (into
        (PriorityQueue. (pm/priority-map) identity)
        [1 2 3])))



(defn dijkstra [graph start-node]
  (let [distances
        (into
         {start-node 0}
         (map
          (juxt identity (constantly Double/POSITIVE_INFINITY))
          (remove #{start-node} (nodes graph))))
        previous (into {} (map (juxt identity (constantly nil))) (nodes graph))
        ;; priority-queue        ;; (into
        ;;  (PriorityQueue. (pm/priority-map-by distances) identity)
        ;;  (remove #{start-node} (nodes graph)))
        q (into [] (remove #{start-node} (nodes graph)))
        ]
    (loop [distances distances
           previous previous
           ;; priority-queue priority-queue
           q q]
      (if-not
          (seq q)
          {:distances distances :previous previous}
          (let [q (sort q)
                current-node (first q)
                neighbors (graph current-node)
                q (rest q)
                f (fn
                    [{:keys [distances] :as state}
                     neighbor]
                    (let [alt (+ (distances current-node) (graph-dist graph current-node neighbor))]
                      (if (< alt (distances neighbor))
                        (-> state
                            (update :distances assoc neighbor alt)
                            (update :previous assoc neighbor current-node)
                            (update :q conj neighbor))
                        state)))
                {:keys
                 [distances previous q]}
                (reduce
                 f
                 {:distances distances :q q :previous previous}
                 neighbors)]
            (recur distances
                   previous
                  q))))))
(dijkstra
 {1 [1 2]
  2 [3]
  3 []}
 1
 )

(defn dijkstra [graph start-node]
  (let [distances (atom {})
        unvisited (atom (into #{} (keys graph)))
        previous (atom {})]
    (swap! distances assoc start-node 0)
    (while (seq @unvisited)
      (let [current-node (first (sort-by (fn [[k v]] v) @distances))]
        (when (contains? graph current-node)
          (doseq [neighbor (keys (graph current-node))]
            (let [alt (+ (distances current-node)
                         (graph current-node neighbor))]
              (when (and (not= neighbor current-node)
                         (or (not (contains? @distances neighbor))
                             (< alt (distances neighbor))))
                (swap! distances assoc neighbor alt)
                (swap! previous assoc neighbor current-node))))))
      (swap! unvisited disj current-node))
    @previous))





(defn dijkstra [graph source]
  (let [dist (atom {})
        q (priority-queue)]
    (swap! dist assoc source 0)
    (doseq [v (keys graph)]
      (when-not (== v source)
        (swap! dist assoc v (float \∞))
        (swap! prev assoc v nil))
      (add-val q v (dist v)))
    (while (not-empty q)
      (let [u (pop q)]
        (doseq [v (keys (graph u))]
          (let [alt (+ (dist u) (graph u v))]
            (when (< alt (dist v))
              (swap! dist assoc v alt)
              (swap! prev assoc v u)
              (add-val q v alt))))))
    {:distances @dist, :previous @prev}))

(defn dijkstra [graph start-node]
  (let [distances
        (into
         {start-node 0}
         (map
          (juxt identity (constantly Double/POSITIVE_INFINITY))
          (remove #{start-node} (nodes graph))))
        previous (into {} (map (juxt identity (constantly nil))) (nodes graph))
        ;; priority-queue
        ;; (into
        ;;  (PriorityQueue. (pm/priority-map-by distances) identity)
        ;;  (remove #{start-node} (nodes graph)))
        q (into [] (remove #{start-node} (nodes graph)))
        ]
    (loop [distances distances
           previous previous
           ;; priority-queue priority-queue
           q q]
      (if-not
          (peek priority-queue)
          {:distances distances :previous previous}
          (let [current-node (peek priority-queue)
                neighbors (graph current-node)
                priority-queue (pop priority-queue)
                f (fn
                    [{:keys [distances] :as state}
                     neighbor]
                    (let [alt (+ (distances current-node) (graph-dist graph current-node neighbor))]
                      (if (< alt (distances neighbor))
                        (-> state
                            (update :distances assoc neighbor alt)
                            (update :previous assoc neighbor current-node)
                            (update :priority-queue conj neighbor))
                        state)))
                {:keys
                 [distances previous priority-queue]}
                (reduce
                 f
                 {:distances distances :priority-queue priority-queue :previous previous}
                 neighbors)]
            (recur distances
                   previous
                   priority-queue))))))
