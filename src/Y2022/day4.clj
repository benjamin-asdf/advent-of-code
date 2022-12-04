(ns Y2022.day4
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   ))

(def
  input
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn part-1 [input]
  (->>
   input
   (re-seq #"(\d+)-(\d+),(\d+)-(\d+)")
   (map #(drop 1 %))
   (map #(map parse-long %))
   (map
    (fn [[a-start a-end b-start b-end]]
      [(into #{} (range a-start (inc a-end)))
       (into #{} (range b-start (inc b-end)))]))
   (filter
    (fn [[elf-a elf-b]]
      (or
       (set/subset? elf-a elf-b)
       (set/subset? elf-b elf-a))))
   count))

(part-1 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day4"))
444


(defn part-2 [input]
  (->>
   input
   (re-seq #"(\d+)-(\d+),(\d+)-(\d+)")
   (map #(drop 1 %))
   (map #(map parse-long %))
   (map
    (fn [[a-start a-end b-start b-end]]
      [(into #{} (range a-start (inc a-end)))
       (into #{} (range b-start (inc b-end)))]))
   (filter
    (fn [[elf-a elf-b]]
      (seq (set/intersection elf-a elf-b))))
   count))


(part-2 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day4"))
801
