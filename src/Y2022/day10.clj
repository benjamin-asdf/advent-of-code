(ns Y2022.day10
  (:require [clojure.string :as str]))

(def input "noop\naddx 3\naddx -5")

(def instruction-set
  {:noop {:cycles 1}
   :addx {:cycles 2 :f (fn [state arg] (update state :x #(+ % arg)))}})

(defn register-states [input]
  (->> input
       (re-seq #"(noop)|(?:(addx) (-?\d+))")
       (map
        (fn [[_ noop? addx? arg]]
          [(keyword (or noop? addx?)) (when arg (parse-long arg))]))
       (map
        #(update % 0 instruction-set))
       (mapcat
        (fn [[{:keys [cycles] :as op} arg]]
          (concat
           (repeat (dec cycles) nil)
           [[op arg]])))
       (reductions
        (fn [state [{:keys [f]} arg]]
          (if-not f state (f state arg)))
        {:x 1})))

(defn signal-strenght [states cycle]
  (* cycle (:x (last (take cycle states)))))

(defn solve-1 [input]
  (->>
   (range 20 (inc 220) 40)
   (transduce
    (map
     (partial signal-strenght (register-states input)))
    +
    0)))

(solve-1 (slurp "inputs/2022/day10-larger"))
13140

(solve-1 (slurp "inputs/2022/day10"))
13760

(defn idx-in-row [x] (mod x 40))

(defn pixel-lit? [cycle x]
  (#{(dec x) x (inc x)} cycle))

(defn render [pixels]
  (->>
   (map #(if % "#" ".") pixels)
   (partition 40)
   (map #(str/join %))))

(defn render-states [input]
  (render
   (map
    #(apply pixel-lit? %)
    (map-indexed
     (fn [i {:keys [x]}]
       [(idx-in-row i)
        (idx-in-row x)])
     (register-states input)))))

(render-states (slurp "inputs/2022/day10"))

#_(
   "###..####.#..#.####..##..###..####.####."
   "...#.#....#.#.....#.#..#.#..#.#....#...#"
   "...#.###..##.....#..#....#..#.###..###.#"
   ".##..#....#.#...#...#....###..#....#...#"
   "..#..#....#.#..#....#..#.#....#....#...#"
   "...#.#....#..#.####..##..#....####.#....")


(count "RFKZCPEF")
