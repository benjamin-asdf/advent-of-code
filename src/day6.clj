(ns day6
(:require [clojure.string :as str]))


(def example "3,4,3,1,2")

(defn fish [input]
  (->>
   (str/split input #",")
   (map str/trim)
   (map parse-long)
   ;; (remove nil?)
   ))

(defn next-fish [curr]
  (into
   []
   (comp
    (map dec)
    (mapcat
     (fn [e]
       (case e
         -1 [6 8]
     e    [e]))))
   curr))

(count (last (take (inc 18) (iterate next-fish (fish example)))))
(count (last (take (inc 80) (iterate next-fish (fish example)))))

(let [input (slurp "inputs/day6")]
  (count
   (last
    (take
     (inc 80)
     (iterate
      next-fish
      (fish input))))))

;; 366057
;; part 2

(time
 (let [
       input  (slurp "inputs/day6")
       ;; input example
       ]
   (->>
    (loop [curr (frequencies (fish input))
           cnt 0]
      (if
          (= cnt 256)
          curr
          (recur
           (let
               [next
                (into
                 {}
                 (map
                  (fn [[k v]] [(dec k) v]))
                 curr)
                next (dissoc next -1)
                spawns (or (curr 0) 0)
                sixes (+ (or (next 6) 0) (or spawns 0))]
               (merge
                next
                {}
                {8 spawns}
                {6 sixes}))
           (inc cnt))))
    vals
    (reduce +))))

;; 1653559299811

;; sick solution
;; https://github.com/mknoszlig/aoc2021/blob/main/src/aoc2021/day6.clj
