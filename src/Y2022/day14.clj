(ns Y2022.day14
  (:require [clojure.string :as str]))

(def input "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9\n")

(defn rocks [rock-line]
  (into #{} (mapcat
             (fn [[[x-start y-start] [x-end y-end]]]
               (for [x (range (min x-start x-end) (inc (max x-start x-end)))
                     y (range (min y-start y-end) (inc (max y-start y-end)))]
                 [x y]))
             (partition 2 1 rock-line))))

(defn
  parse-rocks
  [input]
  (->>
   input
   (str/split-lines)
   (map #(re-seq #"\d+" %))
   (map #(map parse-long %))
   (map #(partition 2 %))
   (mapcat rocks)
   (into #{})))

(defn possible-next-sand [[x y]]
  [[x (inc y)]
   [(dec x) (inc y)]
   [(inc x) (inc y)]])

(defn run-sand [{:keys
                 [rocks
                  resting-sand
                  filled?
                  occupied-fn?
                  current-sand] :as state}]
  (let [[next-x next-y :as next-sand]
        (when
            current-sand
            (first
             (remove
              (occupied-fn? state)
              (possible-next-sand
               current-sand))))]
    (cond
      (filled? state next-sand)
      {:filled state}
      next-sand
      (assoc state :current-sand next-sand)
      ;; next sand
      :else
      (-> state
          (update :resting-sand conj current-sand)
          (assoc :current-sand [500 0])))))

(defn part-1 [input]
  (->>
   (iterate
    run-sand
    {:rocks (parse-rocks input)
     :occupied-fn?
     (fn [{:keys [resting-sand rocks]}]
       (into resting-sand rocks))
     :filled? (fn [{:keys [rocks]} [_ next-y :as next-sand]]
                (when next-sand
                  (< (apply max (map second rocks)) next-y)))
     :resting-sand #{}
     :current-sand [500 0]})
   (filter :filled)
   first
   :filled
   :resting-sand
   count))

;; slow 5+ seconds but I got my stars
(part-1 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day14"))

(defn part-2 [input]
  (->>
   (let [rocks (parse-rocks input )
         bottom-y (+ 2 (apply max (map second rocks)))]
     (iterate
      run-sand
      {:rocks rocks
       :resting-sand #{}
       :filled? (fn [{:keys [resting-sand]} _] (resting-sand [500 0]))
       :occupied-fn?
       (fn [{:keys [resting-sand rocks]}]
         (fn [[_ y :as sand]]
           (or (resting-sand sand)
               (rocks sand)
               (<= bottom-y y))))
       :current-sand [500 0]}))
   (filter :filled)
   first
   :filled
   :resting-sand
   count))

(part-2 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day14"))
