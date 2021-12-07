(ns
    day7
    (:require
     [clojure.string :as str]))

(def example "16,1,2,0,4,2,7,1,2,14")

(defn ->crabs [input] (read-string (format "[%s]" input)))

(defn fuel [position crabs]
  (reduce
   (fn [acc n]
     (+ acc
        (Math/abs
         (- position n))))
   0
   crabs))

(let [crabs (->crabs (slurp "inputs/day7"))]
  (apply
   min
   (map
    #(fuel % crabs)
    (range
     (apply min crabs)
     (inc (apply max crabs))))))

;; part 2

(defn fuel-1* [num]
  (reduce + (range 1 (inc num))))

;; doesn't help, whatever
(def fuel-1 (memoize fuel-1*))

(defn fuel-2
  [position crabs]
  (reduce
   (fn [acc n]
     (+ acc (fuel-1 (Math/abs (- position n)))))
   0
   crabs))


(time (let [crabs (->crabs
              (slurp "inputs/day7")
              )]
   (apply
    min
    (map
     #(fuel-2 % crabs)
     (range
      (apply min crabs)
      (inc (apply max crabs)))))))

;; 89647695

(comment
  (fuel-1* 3))
