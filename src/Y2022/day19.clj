(ns Y2022.day19
  (:require [clojure.string :as str]))

(defn parse-blueprints [input]
  (->>
   (re-seq
    #"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."
    input)
   (map rest)
   (map #(map parse-long %))
   (map
    (fn
      [[_
        ore-cost
        clay-ore
        obsidian-ore
        obsidian-clay
        geode-ore
        geode-obsidian]]
      {:ore-robot {:ore ore-cost}
       :clay-robot {:ore clay-ore}
       :obsidian-robot {:ore obsidian-ore
                        :clay obsidian-clay}
       :geoderobot {:ore geode-ore
                    :obsidian geode-obsidian}}))
   (into [])))

(parse-blueprints (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day19"))


(def blueprints
  [{:ore-robot {:ore 4},
    :clay-robot {:ore 2},
    :obsidian-robot {:ore 3, :clay 14},
    :geoderobot {:ore 2, :obsidian 7}}
   {:ore-robot {:ore 2},
    :clay-robot {:ore 3},
    :obsidian-robot {:ore 3, :clay 8},
    :geoderobot {:ore 3, :obsidian 12}}])

(def start-state
  {:robots {:ore 1 :clay 0 :geode 0 :obsidian 0}
   :resources {:ore 0 :clay 0 :obsidian 0 :geode 0}
   :time 24})

;; if you build, your factory is locked until you step-time
;; and your robot is functional after 1 time
(defn step-time [{:keys [time robots resources building] :as state}]
  (-> state
      (update :time dec)
      (update :resources (fn [m] (merge-with + m robots)))
      (update-in [:robots building] inc)
      (dissoc :building)))

(defn blueprint-molecules [blueprints]
  (map-indexed
   (fn
     [idx b]
     (->
      start-state
      (assoc :blueprint b)
      (assoc :blueprint-n idx)))
   blueprints))

;; chemical computing
;; each blueprint would be
;; a starting kind of molecule I think

;; then, are there "decision" molekules?
;; or should I just step all the molekules and sometimes they decide to split into 2

(def molecules
  ;; I model decisions as molecules
  ;; then the facories/states are bumping into decisions
  ;; if you bump into the descicion to build something
  ;; but you don't have the resources, you wait instead
  ;; time step is a second kind of molecule
  [{:decision [:build :clay]}
   {:decision [:build :ore]}
   {:decision [:build :geode]}
   {:decision [:build :obsidian]}
   {:decision [:dont-build]}
   {:time 1}

   ]
  )

(def factory? :robots)

(defn max-reaction-reducing [[a b]]
  (if (> a b) [a] [a b]))


(defn robot-factory-stepping [[a b]]
  (if-let
      [[factory move]
       (cond
         (and (factory? a)
              (factory? b))
         nil
         (factory? a)
         [a b]
         (factory? b)
         [b a]
         :else nil)]
    ;; apply the move
      '()
      (cond
        (:time move)
        ;; time doesn't get consumed evaporate
        [(step-time factory) move]
        (:building factory)


        )

      ;; incompatible reaction.. flawed model?
      [a b]))





(defn mix-and-react [reaction mols]
  (let [mixed (partition 2 (shuffle mols))]
    (mapcat reaction mixed)))

(def molecules (range 2 101))

(defn reaction-cycle [reaction n]
  (loop
      [i n mols molecules]
      (if
          (zero? i)
          mols
          (recur
           (dec i)
           (mix-and-react reaction mols)))))

(take 10 (mix-and-react max-reaction-reducing molecules))

(let [reactions (reaction-cycle max-reaction-reducing 10000)]
  (-> reactions distinct sort))





;; the momemnt I have enough resources to build a robot,
;; there is a fork in the road I suppose
;; 1. don't build
;; 2. for each robot I /can/ build, build

;;  I can say the acc is the stuff I found out so far
;; then I go


;; then I get the edges of where I can make time go forward still
;; then I split the descicions
;; add it to the queue of possible work
;; recur the function
;; step the time everywhere where I can




;; can I ever build more than 1? Potentially yes
;; there might be a situation of either building 1 or 2 other ones
;; so I should find all possible building combinations
;; nvm, I can only build 1 at a time
