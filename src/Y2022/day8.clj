(ns Y2022.day8
  (:require
   [clojure.core.matrix :as m]
   [clojure.core.matrix.operators :as mo]
   [clojure.string :as str]
   [clojure.set :as set]))

(def input
  "30373
25512
65332
33549
35390")

(defn parse [input]
  (->>
   input
   str/split-lines
   (map (fn [l] (into [] (map parse-long) (re-seq #"\d" l))))
   (into [])
   m/matrix))

(defn
  left-to-right-visibles
  [slice]
  (:visibles
   (reduce
    (fn
      [{:keys [highest visibles] :as state} [coord e]]
      (->
       state
       (assoc :highest (max e highest))
       (assoc :visibles (if (< highest e) (conj visibles coord) visibles))))
    {:highest -1 :visibles #{}}
    slice)))

(defn part-1 [matrix]
  (count
   (let [matrix (m/emap-indexed vector matrix)]
     (->>
      [matrix
       (apply mapv vector matrix)]
      (transduce
       (comp
        (mapcat m/slices)
        (mapcat (juxt identity rseq))
        (map left-to-right-visibles))
       set/union
       #{})))))

(part-1 (parse input))
21


(part-1 (parse (slurp "inputs/2022/day8")))
1854

(defn coord-move [coord direction]
  (mo/+
   coord
   (direction
    {:up [-1 0]
     :down [1 0]
     :left [0 -1]
     :right [0 1]})))

(defn neighbours-1 [m coord direction]
  (take-while
   identity
   (map (fn [coord] (try (apply m/mget m coord) (catch Throwable _)))
        (reductions coord-move coord (repeat direction)))))

(defn score-1 [[tree & trees]]
  (let [tree-count (count (take-while #(< % tree) trees))]
    (if (nth trees tree-count nil)
      (inc tree-count)
      tree-count)))

(defn part-2 [matrix]
  (->>
   (m/emap-indexed
    (fn [coord _e]
      (transduce
       (comp
        (map (fn [dir] (neighbours-1 matrix coord dir)))
        (map score-1))
       *
       1
       [:left :right :up :down]))
    matrix)
   (m/ereduce max)))

(part-2 (parse (slurp "inputs/2022/day8")))
527340
