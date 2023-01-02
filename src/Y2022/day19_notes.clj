(ns Y2022.day19
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))

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
      {:ore {:ore ore-cost}
       :clay {:ore clay-ore}
       :obsidian {:ore obsidian-ore :clay obsidian-clay}
       :geode {:ore geode-ore :obsidian geode-obsidian}}))
   (into[])))

(parse-blueprints (slurp "inputs/2022/day19"))

(def blueprints
  [{:ore {:ore 4},
    :clay {:ore 2},
    :obsidian {:ore 3, :clay 14},
    :geode {:ore 2, :obsidian 7}}
   {:ore {:ore 2},
    :clay {:ore 3},
    :obsidian {:ore 3, :clay 8},
    :geode {:ore 3, :obsidian 12}}])

(def start-state
  {:robots {:ore 1 :clay 0 :geode 0 :obsidian 0}
   :resources {:ore 0 :clay 0 :obsidian 0 :geode 0}
   :time 24})

(let [o (Object.)] (defn log [msg] (locking o (println msg))))

(defn step-time [{:keys [time robots resources building] :as state}]
  (if (zero? time)
    state
    (let [state (-> state
                    (update :time dec)
                    (update :resources (fn [m] (merge-with + m robots))))]
      ;; if you build, your factory is locked until you step-time
      ;; and your robot is functional after 1 time
      (if-not building
        state
        (-> state
            (update-in [:robots building] inc)
            (dissoc :building))))))

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
  [{:build :clay}
   {:build :ore}
   {:build :geode}
   {:build :obsidian}
   ;; I can bias the simulation with the relative amount of molecules
   {:time 1}])

(def factory? :robots)

(defn factory-build-if-possible [{:keys
                                  [blueprint
                                   resources] :as factory} build-kind]
  (let [new-resources
        (merge-with - resources (build-kind blueprint))]
    (if (some neg? (vals new-resources))
      factory
      (-> factory
          (assoc :resources new-resources)
          (assoc :building build-kind)))))

(comment
  (->
   {:robots {:ore 1,
             :clay 0,
             :geode 0,
             :obsidian 0},
    :resources {:ore 0,
                :clay 0,
                :obsidian 0,
                :geode 0},
    :time 24,
    :blueprint {:ore {:ore 4},
                :clay {:ore 2},
                :obsidian {:ore 3, :clay 14},
                :geode {:ore 2, :obsidian 7}},
    :blueprint-n 0}
   step-time
   step-time
   step-time
   step-time
   step-time
   (factory-build-if-possible :ore)
   step-time))

(defn robot-factory-reaction [[a b]]
  (if-let
      [[factory move]
       (cond
         (and (factory? a) (factory? b))
         nil
         (factory? a)
         [a b]
         (factory? b)
         [b a]
         :else
         nil)]
      (cond
        (:time move)
        [(step-time factory) move]
        (and (:building factory) (:build move))
        [factory move]
        (:build move)
        [(factory-build-if-possible factory (:build move)) move]
        ;; healing molecule,
        ;; try to delete factories that are not doing well
        ;; and duplicate factories that are doing well

        )
      [a b]))

(defn mix-and-react [reaction mols]
  (let [mixed (partition 2 (shuffle mols))]
    (def mixed mixed)
    (mapcat reaction mixed)))

(defn reaction-cycle [reaction mols n]
  (loop
      [i n mols mols]
      (if
          (zero? i)
          mols
          (recur
           (dec i)
           (mix-and-react reaction mols)))))

(defn collected-geodes [m] (-> m :resources :geode))

(let [blueprint-mols (blueprint-molecules blueprints)
      mols
      (into
       []
       (concat
        (take 180
              (cycle
               [{:build :ore}
                {:build :clay}
                {:build :geode}
                {:build :geode}
                {:build :geode}
                {:build :obsidian}
                {:build :obsidian}
                {:build :obsidian}
                {:build :obsidian}]))
        (repeat 10 {:time 1})
        (take 10 (cycle blueprint-mols))))
      mols (if (even? (count mols)) mols (into mols [{:time 1}]))
      reactions
      (mapcat identity (pmap (fn [mols] (reaction-cycle robot-factory-reaction mols 1000)) (repeat 100 mols)))]
  (->>
   reactions
   (filter factory?)
   (filter (comp zero? :time))
   (sort-by collected-geodes >)
   (take 10)))


(def top-geodes *1)

(first top-geodes)
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


;; it doesn't work yet, they don't find the best runs
;; I am thinking of adding a "recovery" molecule that resets runs that are not doing well

(let [blueprint-mols [(second blueprints)] ;; (blueprint-molecules blueprints)
      mols
      (into
       []
       (concat
        (take 180
              (cycle
               [{:build :ore}
                {:build :clay}
                {:build :geode}
                {:build :geode}
                {:build :geode}
                {:build :obsidian}
                {:build :obsidian}
                {:build :obsidian}
                {:build :obsidian}]))
        (repeat 30 {:time 1})
        (take 10 (cycle blueprint-mols))))
      mols (if (even? (count mols)) mols (into mols [{:time 1}]))
      reactions
      (mapcat identity (pmap (fn [mols] (reaction-cycle robot-factory-reaction mols 700)) (repeat 250 mols)))
      sorted-reactions
      (->>
       reactions
       (filter factory?)
       (filter (comp zero? :time))
       (sort-by collected-geodes >))]

  (def top-runs
    (for [i (range (count blueprints))]
      (first (filter (comp #{i} :blueprint-n) sorted-reactions))))

  (reduce
   +
   (for [i (range (count blueprints))]
     (* (inc i) (collected-geodes (first (filter (comp #{i} :blueprint-n) sorted-reactions))))))

  [(collected-geodes (first top-runs))
   (collected-geodes (second top-runs))])
*1
