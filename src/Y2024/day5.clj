(ns Y2024.day5
  (:require [clojure.string :as str]
            [clojure.core.async :as a]
            [clojure.core.logic.pldb :as pldb]
            [clojure.core.logic.fd :as fd]
            [clojure.core.logic :as logic :refer :all]))

;; 🎄 ---------------- Day 5  🎄

;; After much trying around and learning some core.logic:

;; ---------------------------------

(defn parse-input
  [s]
  (let [[orderings updates] (str/split s #"\n\n")]
    [(vec (map (fn [os]
                 (vec (map parse-long
                        (str/split os #"\|"))))
            (str/split-lines orderings)))
     (vec (map (comp vec
                     #(map (partial parse-long) %)
                     #(re-seq #"\d+" %))
            (str/split-lines updates)))]))

(defn middle-entry
  [coll]
  (nth coll (Math/floorDiv (long (count coll)) (long 2))))

(pldb/db-rel ord a b)

(logic/defna ordero
             [l]
             ([[b]])
             ([[a b . r]]
              (ord a b)
              (ordero (lcons b r))))

(defn part-1
  [[ordering page-updates]]
  (let [db (apply pldb/db
                  (map (fn [[a b]] [ord a b]) ordering))
        correct? (fn [page-update]
                   (seq
                    (run-db 1 db [q] (ordero page-update))))]
    (reduce +
            (map middle-entry (filter correct? page-updates)))))

(part-1 (parse-input (slurp "/home/benj/repos/advent-of-code/inputs/2024/5/example")))
143

;; -----------------------------------

(defn part-2
  [[ordering page-updates]]
  (let [db (apply pldb/db
             (map (fn [[a b]] [ord a b]) ordering))
        correct?
          (fn [page-update]
            (seq (run-db 1 db [q] (ordero page-update))))
        compare-ordered (fn [b a]
                          (if (correct? [a b]) 1 -1))
        incorrect-pages (remove correct? page-updates)]
    (reduce +
      (map middle-entry
        (map (fn [page-update]
               (sort compare-ordered page-update))
          incorrect-pages)))))

(part-2
 (parse-input
  (slurp
   "/home/benj/repos/advent-of-code/inputs/2024/5/example")))
123

(time
 (part-2
  (parse-input
   (slurp
    "/home/benj/repos/advent-of-code/inputs/2024/5/input"))))
6336
;; "Elapsed time: 1209.385335 msecs"



;; -------------------------
;; this works but is too slow
(defn part-2-too-slow
  [[ordering page-updates]]
  (let [db (apply pldb/db
                  (map (fn [[a b]] [ord a b]) ordering))
        updated-pages
        (doall
         (for [page-update page-updates]
           (let [vars (into []
                            (repeatedly (count
                                         page-update)
                                        lvar))]
             (run-db 1
                     db
                     [q]
                     (conda
                      [(ordero page-update) logic/fail]
                      [(everyg (fn [lv]
                                 (fd/in lv
                                        (apply fd/domain
                                               page-update)))
                               vars)
                       (ordero vars)
                       (== q vars)])))))]
    (reduce +
            (map middle-entry (keep first updated-pages)))))

#_(part-2
 (parse-input (slurp "/home/benj/repos/advent-of-code/inputs/2024/5/example")))
123
