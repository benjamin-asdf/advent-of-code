(ns
    Y2022.calories
    (:require
     [clojure.string :as str]))

(->>
 "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"
 str/split-lines
 (partition-by str/blank?)
 (remove #{'("")})
 (map-indexed
  (fn [i coll]
    {:callories (apply + (map parse-long coll))
     :elf-n i}
    ))
 (apply max-key :callories))

(->>
 (slurp "./inputs/2022/day1")
 str/split-lines
 (partition-by str/blank?)
 (remove #{'("")})
 (map-indexed
  (fn [i coll]
    {:callories (apply + (map parse-long coll))
     :elf-n i}))
 (apply max-key :callories))
{:callories 70369, :elf-n 1}

(->>
 (slurp "./inputs/2022/day1")
 str/split-lines
 (partition-by str/blank?)
 (remove #{'("")})
 (map-indexed
  (fn [i coll]
    {:callories (apply + (map parse-long coll))
     :elf-n i}))
 (map :callories)
 (sort-by identity (fn [a b] (compare b a)))
 (take 3)
 (apply +))
203002
