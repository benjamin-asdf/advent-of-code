(ns
    Y2022.day24
    (:require
     [clojure.string :as str]
     [clojure.core.logic.pldb :as pldb]
     [clojure.core.logic.fd :as fd]
     [clojure.core.logic :as logic]))

(def input "#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#
")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(into [] %))))

(def grid (parse-input input))

(def player [1 j0])

(defn ->blizard [i e]
  (when (#{\< \> \^ \v} e)
    {:pos i :dir e}))

(defn ->blizards [grid]
  (into
   []
   cat
   (map-indexed
    (fn [i row]
      (keep-indexed
       (fn [j e]
         (->blizard [i j] e))
       row))
    grid)))

(logic/defne player-move [player]
  ([[x y]]
   (logic/or*
    [(fd/+ x 1) y]
    [(fd/- x 1) y]
    [x (fd/+ y 1)]
    [x (fd/- y 1)])))

(logic/run 1 [q]
  (logic/== q [0 0])
  (player-move q))
