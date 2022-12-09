(ns Y2022.day9
  (:require
   [clojure.core.matrix :as m]))

(def input "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2")

(defn parse-moves [input]
  (->> input
       (re-seq #"(\w) (\d+)")
       (map (fn [[_ dir n]] [dir (parse-long n)]))))

(defn get-head-positions [moves]
  (reductions
   m/add
   [0 0]
   (sequence
    (comp
     (mapcat
      (fn [[dir n]] (repeat n dir)))
     (map
      {"R" [1 0]
       "L" [-1 0]
       "U" [0 1]
       "D" [0 -1]}))
    moves)))

(defn tail-position [curr-tail-pos head-pos]
  (let [diff
        (m/sub head-pos curr-tail-pos)]
    (if
        (< 1.5 (m/magnitude diff))
        (m/add curr-tail-pos
               (->> (m/normalise diff)
                    (m/emap
                     (fn [i]
                       (cond
                         (< 0.1 i) 1
                         (> -0.1 i) -1
                         :else 0)))))
        curr-tail-pos)))

(defn head-positions->tail-positions [head-positions]
  (reductions tail-position [0 0] head-positions))

;; the last tail positions become the head positions of the next tail
(defn solve [n input]
  (->>
   (get-head-positions (parse-moves input))
   (iterate head-positions->tail-positions)
   (take n)
   last
   (into #{})
   count))

(solve 2 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day9"))
6243

(solve 10 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day9"))
2630
