(ns Y2022.day3
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def input
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")


(defn priority [c]
  (cond
    (<= (int \a) (int c))
    (inc (- (int c) (int \a)))
    :else (+ (- (int c) (int \A)) 27)))

(priority \a)
(priority \b)
(priority \z)
(priority \A)
(priority \B)
(priority \Z)


(defn part-1 [input]
  (->> (str/split-lines input)
       (transduce
        (comp
         (map
          (fn [s]
            (partition
             (/ (count s) 2) s)))
         (map
          (fn [[cp1 cp2]]
            (set/intersection
             (into #{} cp1)
             (into #{} cp2))))
         cat
         (map priority))
        + 0)))

(part-1 (slurp "inputs/2022/day3"))
8109


(defn part-2 [input]
  (->> (str/split-lines input)
       (transduce
        (comp
         (partition-all 3)
         (map
          (fn [[elf1 elf2 elf3]]
            (set/intersection
             (into #{} elf1)
             (into #{} elf2)
             (into #{} elf3))))
         cat
         (map priority))
        + 0)))


(part-2 (slurp "inputs/2022/day3"))
2738
