(ns
    Y2022.day13
    (:require
     [clojure.string :as str]))

(def input "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]")

;; I programmed this using chat gpt

(defn compare-inputs [left right]
  (cond
    (and (integer? left) (integer? right))
    (cond (< left right) :left-first
          (> left right) :right-first
          :else :same)

    (and (seqable? left) (seqable? right))
    (loop [i 0]
      (cond
        (and (= (count left) (count right))
             (every? #{:same} (map compare-inputs left right)))
        :same
        (= i (count left)) :left-first
        (= i (count right)) :right-first
        :else
        (let [c (compare-inputs (nth left i) (nth right i))]
          (if (not= :same c) c
              (recur (inc i))))))

    (integer? left)
    (compare-inputs [left] right)

    (integer? right)
    (compare-inputs left [right])

    :else :same))

(defn part-1 [input]
  (->>
   (read-string (str "[" input "]"))
   (partition 2)
   (map #(apply compare-inputs %))
   (map-indexed vector)
   (filter (comp #{:left-first} second))
   (map first)
   (map inc)
   ;; (1 2 4 6)
   (reduce +)))

(part-1 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day13") )
6369

(defn part-2 [input]
  (let [d-packets #{[[2]]
                    [[6]]}]

    (->>
     (into (read-string (str "[" input "]"))
           d-packets)
     (sort (fn [a b] (case
                         (compare-inputs a b)
                         :left-first -1
                         :right-first 1
                         0)))
     (map-indexed vector)
     (filter (comp d-packets second))
     (map first)
     (map inc)
     (reduce *))))


(time (part-2 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day13")))
"Elapsed time: 8.917616 msecs"

25800
