(ns Y2024.day1
  (:require
   [clojure.data.json :as json]))


(defn list-distances
  [input]
  (let [list-a (map first (partition 2 input))
        list-b (map second (partition 2 input))]
    (reduce +
      (map (comp abs #(- %1 %2))
        (sort list-a)
        (sort list-b)))))

(list-distances
 (read-string (str "[" (slurp "inputs/2024/1/input1") "]")))
1388114

(defn sum-frequencies
  [input]
  (let [list-a (map first (partition 2 input))
        list-b (map second (partition 2 input))
        freqs (frequencies list-b)]
    (transduce (comp (map (juxt identity #(freqs % 0)))
                     (map (fn [[nr freq-in-list-b]]
                            (* nr freq-in-list-b))))
               +
               list-a)))


(sum-frequencies (read-string (str "[" (slurp "inputs/2024/1/input1") "]")))
23529853


(comment
  ;; example:

  (let [input [3 4 4 3 2 5 1 3 3 9 3 3]
        list-a (map first (partition 2 input))
        list-b (map second (partition 2 input))]
    (reduce +
            (map (comp abs #(- %1 %2))
                 (sort list-a)
                 (sort list-b))))
  11

  (let [input [3 4 4 3 2 5 1 3 3 9 3 3]
        list-a (map first (partition 2 input))
        list-b (map second (partition 2 input))
        freqs (frequencies list-b)]
    (transduce
     (comp (map (juxt identity #(freqs % 0)))
           (map (fn [[nr freq-in-list-b]]
                  (* nr freq-in-list-b))))
     +
     list-a))
  31)
