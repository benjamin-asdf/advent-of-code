(ns Y2022.day16
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))

(def input
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

;; parse into a graph with flow rate and neighbors
(defn parse-input [input]
  (->>
   input
   (re-seq
    #"Valve (\w+).+?rate=(\d+);.+valves (.+?)\n")
   (map
    (fn [[_ name rate neighbors]]
      [name (Integer/parseInt rate) (mapv str (read-string (str "[" neighbors "]")))]))
   (into {} (map (juxt first rest)))))

(def graph (parse-input input))

;; All of the valves begin closed. You start at valve AA, but it must be damaged or jammed or something: its flow rate is 0, so there's no point in opening it. However, you could spend one minute moving to valve BB and another minute opening it; doing so would release pressure during the remaining 28 minutes at a flow rate of 13, a total eventual pressure release of 28 * 13 = 364. Then, you could spend your third minute moving to valve CC and your fourth minute opening it, providing an additional 26 minutes of eventual pressure release at a flow rate of 2, or 52 total pressure released by valve CC.

;; Making your way through the tunnels like this, you could probably open many or all of the valves by the time 30 minutes have elapsed. However, you need to release as much pressure as possible, so you'll need to be methodical.


;; I have, 30 min, 1 min to visit, 1 min to open a valve
;; find the path with the maximum preassue release (the sum of the flow rates I visit)
;; in this example


(defn dfs [g visited start]
  (if-not (contains? g start)
    #{}
    (if (contains? visited start)
      #{(cons start visited)}
      (let [v (assoc visited start 1)
            neighbors (->> g start neighbors)]
        (apply
         set/union
         (map
          (partial dfs g v) neighbors))))))

(dfs graph {} "AA")
