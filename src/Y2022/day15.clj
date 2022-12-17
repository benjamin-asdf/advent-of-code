(ns Y2022.day15
  (:require [clojure.string :as str]))

(def
  input
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(defn
  parse-input
  [input]
  (->>
   input
   (re-seq #"x=(-?\d+), y=(-?\d+)")
   (mapcat rest)
   (map parse-long)
   (partition 2)
   (partition 2)))

(defn
  distance
  [[ax ay] [bx by]]
  (+
   (abs (- ax bx))
   (abs (- ay by))))

(defn grid-neighbors [[x y]]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn beacon->coverage [[sx sy :as sensor] beacon]
  (let [sensor-beacon-d (distance sensor beacon)]
    (loop
        [adj-list #{sensor}
         visited #{sensor}
         acc [sensor]]
        (let [next-neighbors
              (remove visited (mapcat grid-neighbors adj-list))
              next-neighbors
              (for [[x y] next-neighbors
                    :let
                    [d (distance [sx sy] [x y])]
                    :when
                    (<= d sensor-beacon-d)]
                [x y])]
          (if-not
              (seq next-neighbors)
              acc
              (recur
               (into #{} next-neighbors)
               (into visited adj-list)
               (concat acc next-neighbors)))))))

(let [sensor-beacon-pairs
      (parse-input input)
      all-coverage (into #{}
                         (mapcat identity
                                 (pmap
                                  (fn [[sensor beacon]] (beacon->coverage sensor beacon))
                                  sensor-beacon-pairs)))
      row (filter (fn [[_ y]] (= 10 y)) all-coverage)
      sensors-and-beacons (into #{} cat sensor-beacon-pairs)]

  (count
   (into #{} (remove sensors-and-beacons row)))
  )
(defn part-1 [input row-n]
  (let [sensor-beacon-pairs
        (parse-input input)
        all-coverage
        (into #{}
              (mapcat identity
                      (pmap
                       (fn [[sensor beacon]] (beacon->coverage sensor beacon))
                       sensor-beacon-pairs)))
        row (filter (fn [[_ y]] (= row-n y)) all-coverage)
        sensors-and-beacons (into #{} cat sensor-beacon-pairs)]
    (count
     (into #{} (remove sensors-and-beacons row)))))
(part-1
 (slurp
  "/home/benj/repos/clojure/aoc/inputs/2022/day15")
 2000000 )
;; fuck it is too slow

((fn [[sensor beacon]] (beacon->coverage sensor beacon))
 (first (parse-input
         (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day15")))
 )


(comment

  (map str/join
       (for  [y (range -5 25)]
         (for [x (range -2 22)]
           (cond
             (= [2 10] [x y]) "B"
             (= [8 7] [x y]) "S"
             ( (into #{} (manhattan-points 0 0 5)) [x y])

             ;; ((into #{}
             ;;        (beacon->coverage [8 7] [2 10])
             ;;        ) [x y])
             "#"
             :else  "."))))
  )

(defn manhattan-points [[x y] d]
  (for [x* (range (- x d) (+ x d 1))
        y* (range (- y d) (+ y d 1))
        :when
        (<=
         (+ (abs (- x* x)) (abs (- y* y)))
         d)]
    [x* y*]))

(defn beacon->coverage [[sx sy :as sensor] beacon]
  (let [sensor-beacon-d (distance sensor beacon)]
    (manhattan-points sx sy sensor-beacon-d)))

(time
 (let
     [sensor-beacon-pairs (parse-input input)
      sensors-and-beacons (into #{} cat sensor-beacon-pairs)]
     (count
      (sequence
       (comp
        (mapcat
         (fn [[sensor beacon]] (beacon->coverage sensor beacon)))
        (filter (fn [[_ y]] (= 10 y)))
        (remove sensors-and-beacons)
        (distinct)
        )
       sensor-beacon-pairs))))

(sequence (distinct) [10 10 11])

(defn part-1 [input row-n]

(time
 (let
     [sensor-beacon-pairs (parse-input input)
      sensors-and-beacons (into #{} cat sensor-beacon-pairs)]
     (count
      (sequence
       (comp
        (mapcat
         (fn [[sensor beacon]] (beacon->coverage sensor beacon)))
        (filter (fn [[_ y]] (= 10 row-n)))
        (remove sensors-and-beacons)
        (distinct)
        )
       sensor-beacon-pairs))))

  ;; (let [sensor-beacon-pairs
  ;;       (parse-input input)
  ;;       all-coverage
  ;;       (into
  ;;        #{}
  ;;        cat
  ;;        (pmap
  ;;         (fn [[sensor beacon]] (beacon->coverage sensor beacon))
  ;;         sensor-beacon-pairs))
  ;;       row (filter (fn [[_ y]] (= 10 row-n)) all-coverage)
  ;;       sensors-and-beacons (into #{} cat sensor-beacon-pairs)]
  ;;   (count
  ;;    (into #{} (remove sensors-and-beacons row))))
  )

(time (part-1 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day15") 2000000))


(first (parse-input (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day15")))


(count
 (into #{}
       (manhattan-points
        '(3729579 1453415)
        (distance
         '(3729579 1453415)
         '(4078883 2522671)))))


(comment
  (require '[clojure.tools.deps.alpha.repl :refer [add-libs]])
  (require '[criterium.core :as criterium])
  (add-libs '{criterium/criterium {:mvn/version "0.4.6"}})

  (criterium/quick-bench
   (distance
    [3729579 1453415]
    [4078883 2522671])
   )


  (criterium/quick-bench
   (count
    (into
     #{}
     (manhattan-points [0 0] 1418560))))
  (criterium/quick-bench
   (count
    (into
     #{}
     (manhattan-points [0 0] (* 100 100)))))


  )



;; you only need to go from (min x) to (max x)
;; and have something that answers effectively
;; whether a point is covered


(deftype BeaconCoverage [[sx sy :as sensor] beacon])
