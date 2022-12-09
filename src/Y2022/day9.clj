(ns Y2022.day9
  (:require
   [clojure.string :as str]
   [clojure.core.matrix.operators :as mo]
   [clojure.core.matrix :as m]))


(m/add [0 0] [0 1])

(def input
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

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
  (reductions
   tail-position
   [0 0]
   head-positions))

(defn part-1 [input]
  (count
   (into #{}
         (head-positions->tail-positions (get-head-positions (parse-moves input))))))

(part-1 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day9"))
6243

(def input-2
"R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

;; the last tail positions become the head positions of the next tail

(count (into #{} (last (take 10 (iterate head-positions->tail-positions (get-head-positions (parse-moves input-2)))))))
36

(count (into #{} (last (take 10 (iterate head-positions->tail-positions (get-head-positions (parse-moves (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day9") )))))))
2630
