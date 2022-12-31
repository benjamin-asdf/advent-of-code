(ns Y2022.day22)

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)
        [board-str path-str] (clojure.string/split lines #"\s+")
        board (mapv (fn [i] (mapv #(if (= \. %) 0 (inc (int \#))) i))
                    (mapv #(vector %) board-str))]
    {:board board :path path-str}))

(defn move [{:keys [board path x y dir]}]
  (let [path (clojure.string/split path #"(?<=[L|R])")]
    (loop [x x y y dir dir path path]
      (if (empty? path)
        {:x x :y y :dir dir}
        (let [[movement direction] (clojure.string/split (first path) #"(?<=[0-9])")
              [dx dy] (case dir
                        0 [1 0]
                        1 [0 1]
                        2 [-1 0]
                        3 [0 -1])
              x' (+ x (int movement))
              y' (+ y (int movement))
              width (count (first board))
              height (count board)
              x'' (mod x' width)
              y'' (mod y' height)
              x''' (if (and (> x'' 0)
                             (< x'' width)
                             (> y'' 0)
                             (< y'' height))
                      x''
                      (case dir
                        0 (if (> x'' 0) x'' (- width))
                        1 (if (> y'' 0) y'' (- height))
                        2 (if (> x'' 0) x'' width)
                        3 (if (> y'' 0) y'' height)))
              y''' (if (and (> x''' 0)
                             (< x''' width)
                             (> y''' 0)
                             (< y''' height))
                      y'''
                      (case dir
                        0 (if (> y''' 0) y''' height)
                        1 (if (> x''' 0) x''' width)
                        2 (if (> y''' 0) y''' (- height))
                        3 (if (> x''' 0) x''' (- width))))]
          (if (get-in board [y''' x'''])
              (recur x y''' dir (rest path))
              (recur x''' y''' (case direction
                                 \R (mod (inc dir) 4)
                                 \L (mod (dec dir) 4))
                            (rest path))))))))

(defn solve [input]
  (let [{:keys [board path] :as parsed} (parse-input input)
        result (move parsed)]
    (+ (* 1000 (:y result)) (* 4 (:x result)) (:dir result))))

(def input "
...#
.#..
#...
....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.
10R5L5R10L4R5L5")

(solve input)
