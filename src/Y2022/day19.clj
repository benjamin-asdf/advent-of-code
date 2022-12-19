(ns Y2022.day19
  (:require [clojure.string :as str]))

(def blueprint1
  {:ore-robot {:ore 4}
   :clay-robot {:ore 2}
   :obsidian-robot {:ore 3 :clay 14}
   :geoderobot {:ore 2 :obsidian 7}})

;; constructing takes 1 min

(def state
  {:robots {:ore 1 :clay 0 :geode 0 :obsidian 0}
   :resources {:ore 0 :clay 0 :obsidian 0 :geode 0}
   :time 24})


(defn step [{:keys [time robots resources] :as state}]
  (-> state
      (update :time dec)
      (update :resources (fn [m] (merge-with + m robots)))))

;; the momemnt I have enough resources to build a robot,
;; there is a fork in the road I suppose
;; 1. don't build
;; 2. for each robot I /can/ build, build

;; can I ever build more than 1? Potentially yes
;; there might be a situation of either building 1 or 2 other ones
;; so I should find all possible building combinations
;; nvm, I can only build 1 at a time



(defn max-geodes [blueprint time {:keys [robots resources]}]
  (if
      (zero? time)
      0
      (let [ore (:ore resources)
            clay (:clay resources)
            obsidian (:obsidian resources)
            options [0
                     (max-geodes blueprint (dec time) (assoc resources :ore (inc ore)))]
            options (conj options (max-geodes blueprint (dec time) (assoc resources :clay (inc clay))))]
        (if
            (contains? blueprint :obsidian-collecting-robot)
            (let [cost (:obsidian-collecting-robot blueprint)]
              (conj
               options
               (max-geodes
                blueprint
                (dec time)
                (assoc
                 resources
                 :ore
                 (inc (- ore (:ore cost)))
                 :clay
                 (inc (- clay (:clay cost)))))))
            (if
                (contains?
                 blueprint
                 :geode-collecting-robot)
                (let [cost (:geode-collecting-robot
                            blueprint)]
                  (conj
                   options
                   (max-geodes
                    blueprint
                    (dec time)
                    (assoc
                     resources
                     :ore
                     (inc (- ore (:ore cost)))
                     :obsidian
                     (inc
                      (- obsidian (:obsidian cost))))))))
            (apply max options)))))
