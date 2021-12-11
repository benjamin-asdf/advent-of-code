(ns
    day9
  (:require
   [clojure.set :as set]
   [clojure.core.matrix :as m]
   [clojure.string :as str]))

(def
  example
  "2199943210
3987894921
9856789892
8767896789
9899965678")

(defn positions [input]
  (->>
   (str/trim input)
   str/split-lines
   (into
    []
    (comp
     (map seq)
     (map (fn [row] (into [] (map #(parse-long (str %)) row))))))))

(defn low-points [input]
  (let [matrix (positions input)
        [rows colls] (m/shape matrix)
        views
        (for
            [n (range rows)
             j (range colls)]
            (let [selection
                  [[n j]
                   [(inc n) j]
                   [(dec n) j]
                   [n (inc j)]
                   [n (dec j)]]
                  selection
                  (filter (fn [[x y]]
                            (and
                             (> rows x -1)
                             (> colls y -1))) selection)]

              (m/select-indices matrix selection)))]
    (->>
     views
     (into
      []
      (keep
       (fn [[elm & _ :as view]]
         (when
             (and (some (complement #{elm}) view)
                  (= elm (apply min view)))
             elm)))))))

(reduce + (map inc (low-points example)))

(reduce + (map inc (low-points (str/trim (slurp "inputs/day9")))))
;; 524


;; part 2

(defn view [matrix [n j]]
  (let [[rows colls] (m/shape matrix)
        selection
        [[n j]
         [(inc n) j]
         [(dec n) j]
         [n (inc j)]
         [n (dec j)]]
        selection
        (filter (fn [[x y]]
                  (and
                   (> rows x -1)
                   (> colls y -1))) selection)]
    (remove
     (fn [[x y]] (= 9 (m/select matrix x y)))
     selection)))

(defn basin [{:keys [matrix positions new-positions] :as opts}]
  (if (not (seq new-positions))
    {:basin positions}
    (let [next-positions
          (into positions (mapcat #(view matrix %)) new-positions)
          next-new
          (set/difference next-positions positions)]
      (assoc
       opts
       :new-positions next-new
       :positions next-positions))))

(defn position-value [matrix [x y]]
  (m/select matrix x y))

(let [matrix
      (positions (str/trim (slurp "inputs/day9")))
      ;; (positions example)
      ]
  (->>
   (loop [[position & more-positions] (m/index-seq matrix)
          basins []]
     (if-not
         position
         basins
         (let
             [basin
              (when-not
                  (or
                   (some
                    (fn
                      [in-basin?]
                      (in-basin? position))
                    basins)
                   (= 9 (position-value matrix position)))
                  (some
                   :basin
                   (iterate
                    basin
                    {:matrix matrix
                     :positions #{position}
                     :position position
                     :new-positions #{position}})))]
             (recur
              more-positions
              (if basin (conj basins basin) basins)))))
   (map count)
   sort
   reverse
   (take 3)
   (reduce *)))

;; 1235430

;; borkdude https://gist.github.com/borkdude/b5cf0e9d2d8ab7c678d88e27a3357b33#file-aoc21_d09-clj















;;  below corpus mortorem
;;  stuff I tried and thought about

(comment
  (defn right [[x y]]
    [(inc x) y])

  (defn down [[x y]]
    [x (inc y)])

  ;; (defn left [[x y]]
  ;;   [(dec x) y])

  ;; (defn right [[x y]]
  ;;   [[x (dec y)]])


  (defn
    out-off-bounds?
    [matrix [x y]]
    (let [[rows colls] (m/shape matrix)]
      (not
       (and
        (> rows x -1)
        (> colls y -1)))))

  (defn
    basin-edge?
    [matrix [x y]]
    (or
     (out-off-bounds?
      matrix
      [x y])
     (= 9 (m/select matrix x y))))

  (defn grow
    [{:keys [step matrix position acc edge?] :as opts}]
    (when-not (basin-edge? matrix position)
      (let [next-pos (step position)]
        (prn
         "grow: "
         position
         "next: "
         next-pos
         "out of bounds: "
         (out-off-bounds?
          matrix
          next-pos))
        (if-not
            (or
             (out-off-bounds? matrix next-pos)
             (edge? matrix next-pos))
            (recur
             (assoc
              opts
              :acc
              (conj acc next-pos)
              :position next-pos))
            acc))))

  (defn grow-basin-to-edges-1 [matrix position]
    (concat
     (grow
      {:step right
       :matrix matrix
       :position position
       :acc [position]})
     (grow
      {:step down
       :matrix matrix
       :position position
       :acc [position]})))

  (defn grow-basin-to-edges [{:keys [new-positions whole-basin matrix]}]
    (if (seq new-positions)
      (conj whole-basin (mapcat  grow-basin-to-edges-1 positions))))

  (defn
    basins
    [{:keys [all-basins matrix]
      :as opts}
     position]
    (if
        (some
         (fn
           [in-basin?]
           (in-basin? position))
         all-basins)
        opts
        (assoc
         opts
         :all-basins
         (conj
          all-basins
          (into
           #{}
           (concat
            (grow
             {:step right
              :matrix matrix
              :position position
              :acc [position]})
            (grow
             {:step down
              :matrix matrix
              :position position
              :acc [position]})))))))

  ;; you have a view and you want to grow it on all edges
  ;; and if that returns nil you are done

)
