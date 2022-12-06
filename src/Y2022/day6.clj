(ns
    Y2022.day6
    (:require
     [clojure.string :as str]))

(def input "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(defn part-1 [input]
  (->>
   (partition 4 1 input)
   (keep-indexed
    (fn [i elements]
      (when (= (count elements)
               (count (into #{} elements)))
        (+ 4 i))))
   first))

(part-1 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day6"))

1766

;; part 2

(defn find-first-unique [n input]
  (->>
   (partition n 1 input)
   (keep-indexed
    (fn [i elements]
      (when (= (count elements)
               (count (into #{} elements)))
        (+ n i))))
   first))


(find-first-unique 14 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day6"))
2383
