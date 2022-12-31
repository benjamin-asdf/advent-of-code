(ns Y2022.day17
  (:require
   [clojure.string :as str]
   [clojure.core.matrix :as m]
   [clojure.set :as set]))


(m/shift [[1 0] [1 0] [0 0]] 1 (- 1))
[[0 1] [0 1] [0 0]]
(m/shift [[0 1] [0 1] [0 0]]
         ;; ... ?
         )

[[0 0]
 [0 1]
 [0 1]]






(m/shift [[1 0] [0 0]] 2 (- 1))

(def jet-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
;; Define the shapes of the rocks as a list of 2D arrays.

(def shapes
  [[[1 1 1 1]]
   [[0 1 0]
    [1 1 1]
    [0 1 0]]
   [[0 0 1]
    [0 0 1]
    [1 1 1]]
   [[1]
    [1]
    [1]
    [1]]
   [[1 1]
    [1 1]]])
;; Define the function to simulate the movement of the rocks.

(count ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

;; grid stars as []

(def chamber
  [[0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0]])

(defn place-rock [chamber pos shape]
  (let [new-chamber (mapv (fn [row]
                            (mapv (fn [cell]
                                    (if (and (pos :y) (pos :x)) cell shape))
                                  row))
                          chamber)]
    (assoc new-chamber :falling? true)))

(defn collision? [chamber pos shape]
  (let [x (pos :x)
        y (pos :y)]
    (or (or (not (chamber y x))
            (not (chamber (inc y) x)))
        (or (not (chamber (dec y) x))
            (not (chamber y (inc x)))))))


(defn move-rock [chamber pos shape]
  (if (collision? chamber pos shape)
    chamber
    (update-in chamber [pos :y (pos :x)] shape)))


(defn simulate-rocks [jet-input shapes]
  (loop [chamber (vec (repeat 3 (vec (repeat 7 0))))
         jet 0]
    (if (empty? shapes)
      chamber
      (let [shape (first shapes)
            pos {:x 2 :y (- 3 (count (filter #(= 1 %) chamber)))}
            new-chamber (place-rock chamber pos shape)]
        (recur (move-rock new-chamber pos shape)
               (inc jet)
               (rest shapes))))))

(defn collision? [chamber pos shape]
  (some
   (m/mget
    [[0 0 0] [0 0 0] [1 1 1]]
    0
    ;; pos x
    0
    ;; pox y
    ;; matrix from shape
    )
   )

  )


(defn collision? [chamber pos shape]
  (let [x (first pos)
        y (second pos)
        shape-matrix (m/matrix shape)
        chamber-matrix (m/matrix chamber)
        shape-matrix-with-pos (m/mget shape-matrix x y)]
    (some #(= % 1) (m/mul chamber-matrix shape-matrix-with-pos))))

(defn collision? [chamber pos shape]
  (let [x (first pos)
        y (second pos)
        shape-matrix (m/matrix shape)
        chamber-matrix (m/matrix chamber)
        shape-matrix-with-pos (m/reshape shape-matrix [(count chamber) (count (first chamber))])]
    (some #(= % 1) (m/mul chamber-matrix (m/mget shape-matrix-with-pos x y)))))


(def chamber [])
(def chamber [[0 0] [0 0]])
(def pos [1 1])

(def shape [1])

(m/shift
 (m/reshape
  shape
  [(count chamber)
   (count (first chamber))])
 (- 1)
 (- 1))


(m/i
 (m/reshape
  shape
  [(count chamber)
   (count (first chamber))])
 (- 1)
 (- 1))

[[0 0] [0 0]]

(m/mul [[0 0] [0 0]] (m/matrix [1]))

(m/shift [[1 0] [0 0]] 1 1)

(->
 (m/reshape shape [(count chamber) (count (first chamber))])
 (m/shift 1 (- (first pos)))
 (m/shift 2 (- (second pos))))


(m/shift [[1 0] [0 0]] 1 (- 1))

(m/shift [[1 0] [0 0]] 2 (- 1))


(m/zero-matrix 4 4)


(defn simulate-rocks [{:keys [grid jet falling? pos]}]

  (if  (collision? chamber pos)
    ;;
    )

  (case (get jet-input jet-index)
    ;; If the jet direction is ">", move the rock to the right by one unit.
    ">" (recur (update-in pos [:x] inc))
    ;; If the jet direction is "<", move the rock to the left by one unit.
    "<" (recur (update-in pos [:x] dec)))
  )


(defn simulate-rocks [shapes jet-input]
  ;; Initialize the chamber as a map with keys :width, :height, and :rocks.
  ;; Set the width and height to 7 and 100, respectively.
  ;; Set the rocks to an empty list.
  (loop [chamber {:width 7 :height 100 :rocks []}
         ;; Initialize the current position of the rock as a map with keys :x and :y.
         ;; Set the x and y coordinates to 2 and 3, respectively.
         (loop [pos {:x 2 :y 3}]
           ;; Initialize the index of the current shape to 0.
           (loop [shape-index 0]
             ;; Initialize the index of the current jet direction to 0.
             (loop [jet-index 0]
               ;; Initialize the falling flag to true.
               (loop [falling? true]
                 ;; Iterate over the height of the chamber.
                 (loop [h (:height chamber)]
                   ;; If the falling flag is true, move the rock down by one unit.
                   (when falling?
                     (recur (update-in pos [:y] inc)))
                   ;; If the rock has reached the floor or another rock, set the falling flag to false.
                   (when (or (= (:y pos) 0) (collision? chamber pos))
                     (recur (assoc pos :y 0) (falling? false)))
                   ;; If the falling flag is false, push the rock in the direction indicated by the current jet direction.
                   (when (not falling?)
                     )
                   ;; Increment the jet index. If it exceeds the length of the jet-input string, reset it to 0.
                   (if (>= (inc jet-index) (count jet-input))
                     (recur (jet-index 0))
                     (recur (jet-index (inc jet-index)))))
                 ;; If the rock has not moved, add it to the chamber.
                 (when (not (collision? chamber pos))
                   (recur (update-in chamber [:rocks] conj pos))
                   ;; Reset the current position to the starting position.
                   (recur (assoc pos :x 2 :y 3))))))))))



(defn pad-with-zeros [matrix left-pad total-length]
  (mapv #(into [] (concat (repeat left-pad 0) % (repeat (- total-length left-pad (count %)) 0))) matrix))

(defn spawn-rock [grid shape]
  ;; add 3 lines to grid
  ;; grid is 7 units wide
  (let [grid
        (into
         grid
         (repeat 3 (into [] (repeat 7 0))))
        grid (into grid (pad-with-zeros shape 2 7))]
    grid))

(spawn-rock [] (first shapes))
