(ns Y2022.day23
  (:require
   [clojure.string :as str]))

(def input ".....
..##.
..#..
.....
..##.
.....")

(->> input (str/split-lines) (map seq) (mapv #(mapv (fn [i] (case i \# 1 \. 0)) %)))

(def elves
  [[0 0 0 0 0]
   [0 0 1 1 0]
   [0 0 1 0 0]
   [0 0 0 0 0]
   [0 0 1 1 0]
   [0 0 0 0 0]])

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)
        height (count lines)
        width (if (not= 0 height) (count (first lines)))
        elves (vec (mapcat (fn [line] (map-indexed (fn [i c] [(inc i) c]) (seq line))) lines))]
    {:elves elves :height height :width width}))

(defn rotate-directions [directions]
  (let [n (count directions)
        first (first directions)
        rest (rest directions)]
    (concat rest [first])))

(defn move-elves [elves]
  (let [directions ["N" "NE" "NW" "S" "SE" "SW" "W" "E"]
        directions-by-elf (group-by first elves)
        move-proposals (for [dir directions]
                         (for [elf (directions-by-elf dir)]
                           [(inc (first elf)) (second elf) (last elf)]))
        moves (into {} (for [move (flatten move-proposals)
                           :when (not (get moves (second move) false))]
                        [(second move) (last move)]))]
    (vec (for [[x y elf] elves]
            [(inc x) (inc y) (get moves [x y] elf)]))))

(defn run-round [state]
  (let [elves (:elves state)
        directions (:directions state)
        new-elves (move-elves elves)
        new-directions (rotate-directions directions)]
    {:elves new-elves :directions new-directions}))

(defn run-simulation [input]
  (let [state (assoc (parse-input input) :directions ["N" "NE" "NW" "S" "SE" "SW" "W" "E"])
        elves (:elves state)]
    (loop [state state]
      (if (= (count (set (map last elves))) 1)
        state
        (recur (run-round state))))))

(defn solve [input]
  (let [result (run-simulation input)
        elves (:elves result)]
    (count (filter #(= (last %) "#") elves))))
