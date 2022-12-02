(ns Y2022.day2
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))


(def s
 "A Y
B X
C Z")

(def wins?
  {:rock :scissors
   :paper :rock
   :scissors :paper})

(def points
  {:rock 1
   :paper 2
   :scissors 3})

(defn parse-input [s]
  (->>
   (re-seq #"(\w) (\w)" s)
   (sequence
    (comp
     (map #(drop 1 %))
     (map #(map first %))
     (map
      (fn [[left right]]
        [({\A :rock \B :paper \C :scissors} left)
         ({\X :rock \Y :paper \Z :scissors} right)]))))))

(defn play [[left right]]
  (+
   (points right)
   (cond
     (= (wins? right) left) 6
     (= left right) 3
     :else 0)))

(play [:rock :paper])
(->>
 s
 parse-input
 (map play)
 (reduce +))
15

(->>
 (slurp "inputs/2022/day2")
 parse-input
 (map play)
 (reduce +))
13565

;; part 2

(let [loses? (set/map-invert wins?)]
  (defn outcome->move [[left outcome]]
    (case outcome
      :draw left
      :win (loses? left)
      :loose (wins? left))))

(let [s (slurp "inputs/2022/day2")]
  (->>
   (re-seq #"(\w) (\w)" s)
   (sequence
    (comp
     (map #(drop 1 %))
     (map #(map first %))
     (map
      (fn [[left right]]
        [({\A :rock \B :paper \C :scissors} left)
         ({\X :loose \Y :draw \Z :win} right)]))
     (map
      (juxt first outcome->move))
     (map play)))
   (reduce +)))
12424
