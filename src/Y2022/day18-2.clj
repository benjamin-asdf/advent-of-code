(ns Y2022.day18-2
  (:require [clojure.core.matrix :as m]
            [clojure.string :as str]))

(def sample-lava [[2 2 2] [1 2 2] [3 2 2] [2 1 2] [2 3 2] [2 2 1] [2 2 3] [2 2 4] [2 2 6] [1 2 5] [3 2 5] [2 1 5] [2 3 5]])

(defn expand-lava [lava]
  (let [lava? (into #{} lava)
        x-size (reduce max (map first lava))
        y-size (reduce max (map second lava))
        z-size (reduce max (map last lava))]
    (into [] (for [x (range (inc x-size))]
               (into [] (for [y (range (inc y-size))]
                          (into [] (for [z (range (inc z-size))]
                                     (if (lava? [x y z]) 1 0)))))))))

(def lava? #{1})
(def air? (complement lava?))
(def air-cube? (fn [matrix p] (air? (apply m/mget matrix p))))

(defprotocol UnionFind
  (parent [this x]
    "Returns the parent of x. Guaranteed to be non-nil.
    If x is a root then the parent of x is x.")
  (rank [this x]
    "Return the rank of x in the current UnionFind.
    The rank measures the height of the tree at the given element.")
  (find-root [this x]
    "Returns the root of the given element.")
  (union [this x y]
    "Merges the subtrees of the trees that x and y belong to."))

;; http://fhur.github.io/data-structure/clojure/2016/04/01/implementing-immutable-union-find-with-clojure.html
(defrecord RankedUnionFind
    [
     ;; every element must have a parent at all times.
     parent-map
     ;; rank-map holds a mapping of element => rank.
     ;; Every element has a rank >= 0
     rank-map]
    UnionFind
    (parent [this x]
      ;; Simply return whatever is mapped by the parent-map
      (get parent-map x))

    (rank [this x]
      ;; Simply return whatever is mapped by the rank-map
      (get rank-map x))

    (find-root [this x]
      ;; start at the given node and recursively
      ;; iterate through all the node's parents until a root
      ;; is found. This operation takes O(log(n)) = O(height(x)) = O(max rank)
      (loop [node x]
        (let [parent* (parent this node)]
          ;; guaranteed to happen eventually since we are traversing a tree
          (if (= parent* node)
            node
            (recur parent*)))))

    (union [this x y]
      (let [root-x (find-root this x)
            root-y (find-root this y)]
        (cond
          (= root-x root-y)
          this
          (> (rank this root-x) (rank this root-y))
          (new RankedUnionFind (assoc parent-map root-y root-x) rank-map)
          (< (rank this root-x) (rank this root-y))
          (new RankedUnionFind (assoc parent-map root-x root-y) rank-map)
          :else
          (RankedUnionFind. (assoc parent-map root-x root-y)
                            (update rank-map root-y inc))))))

(defn create-ranked-union-find
  "Take a collection of elements and initialize a RankedUnionFind with every
  element mapped to a singleton."
  [coll]
  (let [parents (mapcat (fn [x] [x x]) coll)
        ranks (mapcat (fn [x] [x 0]) coll)]
    (RankedUnionFind. (apply hash-map parents)
                      (apply hash-map ranks))))

(defn
  point->neighbors
  [[x y z]]
  (for
      [[dx dy dz]
       [[1 0 0]
        [-1 0 0]
        [0 1 0]
        [0 -1 0]
        [0 0 1]
        [0 0 -1]]]
      (let [nx (+ x dx)
            ny (+ y dy)
            nz (+ z dz)]
        [nx ny nz])))

(defn
  get-uf
  [matrix inside-matrix?]
  (let [uf (create-ranked-union-find
            (m/index-seq matrix))]
    (m/ereduce
     (fn
       [uf {p :idx e :e}]
       (if
           (lava? e)
           uf
           (reduce
            (fn
              [uf np]
              (cond
                (not (inside-matrix? np))
                uf
                (air-cube? matrix np)
                (union uf p np)
                :else
                uf))
            uf
            (point->neighbors p))))
     uf
     (m/emap-indexed
      (fn [idx e] {:idx idx :e e})
      matrix))))

(defn
  get-air-pockets-connected-to-outside
  "Return the roots of air that is connected outside."
  [matrix uf inside-matrix?]
  (->>
   (m/index-seq matrix)
   (into
    #{}
    (comp
     ;; these are the points that thouch outside
     (filter
      (fn [p]
        (some (complement inside-matrix?) (point->neighbors p))))
     (remove (comp lava? #(apply m/mget matrix %)))
     (map (fn [air-cube] (find-root uf air-cube)))))))

(defn
  count-sides-with-union-find
  [matrix]
  (let [inside-matrix?
        (into #{} (m/index-seq matrix))
        uf (get-uf matrix inside-matrix?)
        air-pockets-connected-to-outside (get-air-pockets-connected-to-outside matrix uf inside-matrix?)]
    (->>
     (m/index-seq matrix)
     (sequence
      (comp
       (filter (comp lava? (apply m/mget matrix %)))
       (mapcat point->neighbors)
       (map
        (fn
          [np]
          (cond
            ;; a neibhour can be outside, counts as air face
            (not (inside-matrix? np))
            :air-face
            ;; the neighbor is lava
            (lava? (apply m/mget matrix np))
            :lava-face
            ;; the neighbor is air connected to the outside
            (air-pockets-connected-to-outside (find-root uf np))
            :air-face)))
       (filter #{:air-face})))
     count)))

(count-sides-with-union-find matrix)
(count-sides-with-union-find (m/matrix (expand-lava sample-lava)))

(time (count-sides-with-union-find
  (m/matrix
   (expand-lava (into [] (mapv read-string (map #(format "[%s]" %) (str/split-lines (slurp "inputs/2022/day18")))))))))
2582
"Elapsed time: 247.248754 msecs"


;; (let [matrix (m/matrix (expand-lava sample-lava))
;;       ;; inside-matrix? (into #{} (m/index-seq matrix))
;;       ;; uf (get-uf matrix  inside-matrix?)
;;       ]
;;   ;; [(find-root uf [2 2 5])
;;   ;;  (find-root uf [0 0 0])]

;;   ;; (get-air-pockets-connected-to-outside matrix uf inside-matrix?)

;; (let [inside-matrix?
;;       (into #{} (m/index-seq matrix))
;;       uf (get-uf matrix inside-matrix?)
;;       air-pockets-connected-to-outside (get-air-pockets-connected-to-outside matrix uf inside-matrix?)]

;;   (->>
;;    (m/index-seq matrix)
;;    (sequence
;;     (comp
;;      (filter (comp lava? #(do (def p %) ( apply m/mget matrix %))))
;;      (mapcat point->neighbors)
;;      (map
;;       (fn
;;         [np]
;;         (cond
;;           ;; a neibhour can be outside, counts as air face
;;           (not (inside-matrix? np))
;;           :air-face
;;           ;; the neighbor is lava
;;           (lava? (apply m/mget matrix np))
;;           :lava-face
;;           ;; the neighbor is air connected to the outside
;;           (air-pockets-connected-to-outside (find-root uf np))
;;           :air-face)))
;;      (filter #{:air-face})))
;;    count)
;;   ))

;; (let [matrix (m/matrix (expand-lava sample-lava))] (m/mget matrix 2 2 6))
