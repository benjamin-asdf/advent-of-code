(ns Y2022.day18
  (:require [clojure.core.matrix :as m]
            [clojure.string :as str]))

(def sample-lava [[2 2 2] [1 2 2] [3 2 2] [2 1 2] [2 3 2] [2 2 1] [2 2 3] [2 2 4] [2 2 6] [1 2 5] [3 2 5] [2 1 5] [2 3 5]])

(defn expand-lava [lava]
  (let [lava? (into #{} lava)
        x-size (reduce max (map first lava))
        y-size (reduce max (map second lava))
        z-size (reduce max (map last lava))]
    (into [] (for [x (range (inc x-size))]
               (into [] (for [y (range (inc y-size))]
                          (into [] (for [z (range (inc z-size))]
                                     (if (lava? [x y z]) 1 0)))))))))

(def matrix
  (m/matrix
   (expand-lava sample-lava)))

(defn neighbors-1 [[x y z]]
  [[(dec x) y z]
   [(inc x) y z]
   [x (inc y) z]
   [x (dec y) z]
   [x y (dec z)]
   [x y (inc z)]])

(def lava? #{1})

(defn count-sides-part-1 [matrix]
  (m/ereduce
   +
   (m/emap-indexed
    (fn [idx e]
      (if-not
          (lava? e)
          0
          (count
           (filter
            #{0}
            (map
             (fn [[x y z]]
               (try
                 (m/mget matrix x y z)
                 (catch Throwable _
                   0)))
             (neighbors-1 idx))))))
    matrix)))

(count-sides-part-1 (m/matrix (expand-lava sample-lava)))

;; (defn outside-1? [matrix [x y z]]
;;   (try (do (m/mget matrix x y z) false) (catch Throwable t true)))

;; (defn air? [matrix [x y z]]
;;   (zero? (try (m/mget matrix x y z) (catch Throwable _ 0))))

;; dfs until I either run out of air or run into the outside

;; (def air-pocket-or-connected-to-outside?
;;   (memoize
;;    (fn  [matrix start]
;;      (loop [visited #{}
;;             to-visit [start]]
;;        (if (empty? to-visit)
;;          {:air-pocket? visited}
;;          (let [current (peek to-visit)
;;                neighbors (remove
;;                           visited
;;                           (filter
;;                            (fn [p] (air? matrix p))
;;                            (neighbors-1 current)))
;;                new-to-visit (into (pop to-visit) neighbors)]
;;            (if
;;                (some #(outside-1? matrix %) new-to-visit)
;;                {:connected-to-outside? (into visited new-to-visit)}
;;                (recur (conj visited current) new-to-visit))))))))

;; (defn search-air-fn [matrix]
;;   (fn
;;     [state neighbor]
;;     (let [pockets (air-pocket-or-connected-to-outside?
;;                                  matrix
;;                                  neighbor)]
;;       (->
;;        state
;;        (update :connected-to-outside? #(into % (:connected-to-outside? pockets)))
;;        (update :air-pocket? #(into % (:air-pocket? pockets)))))))

;; (defn count-sides-part-2 [matrix]
;;   ;; count where the sides are air
;;   (:air-sides
;;    (m/ereduce
;;     (fn [{:keys [connected-to-outside? air-pocket?] :as state} {:keys [idx e]}]
;;       (if-not
;;           (lava? e)
;;           state
;;           (let [neighbors (neighbors-1 idx)
;;                 air-neighbors (filter (fn [p] (air? matrix p)) neighbors)
;;                 known-connected-outside (into #{} (filter connected-to-outside? air-neighbors))
;;                 known-air-pockets (into #{} (filter air-pocket? connected-to-outside?))
;;                 unkown-air-neighbors (remove
;;                                       known-air-pockets
;;                                       (remove
;;                                        known-connected-outside
;;                                        air-neighbors))]
;;             (if
;;                 (empty? air-neighbors)
;;                 state
;;                 (let [{:keys [connected-to-outside? air-pocket?] :as new-state}
;;                       (reduce (search-air-fn matrix) state unkown-air-neighbors)]
;;                   (update new-state :air-sides + (count (remove air-pocket? air-neighbors))))))))
;;     {:air-pocket? #{}
;;      :air-sides 0
;;      ;; these are known air connected air cubies
;;      :connected-to-outside? #{}}
;;     (m/emap-indexed (fn [idx e] {:idx idx :e e}) matrix))))


;; (count-sides-part-2 matrix)

;; (m/shape (m/matrix
;;           (expand-lava (into [] (mapv read-string (map #(format "[%s]" %) (str/split-lines (slurp "inputs/2022/day18"))))))))


;; ok it is to slow, let's try a UF datastructure
;; http://fhur.github.io/data-structure/clojure/2016/04/01/implementing-immutable-union-find-with-clojure.html
