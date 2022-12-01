(ns
    day9
  (:require
   [clojure.set :as set]
   [clojure.core.matrix :as m]
   [clojure.string :as str]))


(def
  example
  "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(defn matrix [input]
  (->>
   (str/trim input)
   str/split-lines
   (into
    []
    (comp
     (map seq)
     (map (fn [row] (into [] (map #(parse-long (str %)) row))))))))

(def input (matrix example))

(defn adjacent-coordinates [x y]
  )

(defn neighbours [])


(defn long-running []
  (Thread/sleep 1000)
  10)

(deref (future (long-running)) 100 :timeout)
