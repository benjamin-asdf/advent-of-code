(ns Y2024.day7-core-logic
  (:require [clojure.string :as str]
            [clojure.core.async :as a]
            [clojure.core.logic.pldb :as pldb]
            [clojure.core.logic.fd :as fd]
            [clojure.core.logic :as logic :refer :all]))

(def example-input
  "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn parse-input [s]
  (into
   [] (map
       (comp
        vec
        (partial map parse-long)
        #(re-seq #"\d+" %1))
       (clojure.string/split-lines s))))


;; ---------------
;; bespoke core.logic

(defn run-equation
  [target-v a inputs operators]
  (if-not (seq inputs)
    (== a target-v)
    (and*
     ;; is an attempt at perf but doesn't make much difference
     [(fd/<= a target-v)
      (or* (for [op operators]
             (run-equation target-v
                           (op a (first inputs))
                           (rest inputs)
                           operators)))])))

(defn runs?
  [operators [target-value a & inputs]]
  (run 1 [q] (== q target-value) (run-equation q a inputs operators)))

(defn part-1 [input]
  (reduce + (mapcat (partial runs? [+ *]) (parse-input input))))

(defn part-2
  [input]
  (reduce +
    (mapcat (partial runs?
                     [+ *
                      (fn concat-op [a b]
                        (parse-long (str a b)))])
            (parse-input input))))

(part-1 example-input)

(time
 (part-1
  (slurp
   "/home/benj/repos/advent-of-code/inputs/2024/7/input")))
1298103531759

;; "Elapsed time: 119.655255 msecs"

(time
 (part-2
  (slurp
   "/home/benj/repos/advent-of-code/inputs/2024/7/input")))
140575048428831
;; "Elapsed time: 4409.906271 msecs"
;; "Elapsed time: 4555.854874 msecs"



(comment
  (mapcat (partial runs? [+ *]) (parse-input example-input))
  (190 3267 292))
