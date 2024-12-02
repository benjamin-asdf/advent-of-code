(ns Y2024.day2)

;;

(def example-input
  "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9")

(defn parse-inputs
  [s]
  (map read-string
    (map (fn [line] (str "[" line "]"))
      (clojure.string/split s #"\n"))))

(def level [7 6 4 2 1])

(defn differences
  [level]
  (map (fn [[a b]] (- a b)) (partition 2 1 level)))

(defn differences-safe?
  [differences]
  (if-not (seq differences)
    true
    (and
     ;; 1. sign of the differences is the same
     (= 1 (count (group-by pos? differences)))
     ;; 2. magnitute of the differences is between 1-3
     (every? (fn [difference] (<= 1 (abs difference) 3))
             differences))))

(defn safe?
  [level]
  (let [differences (differences level)]
    (and
      ;; 1. sign of the differences is the same
      (= 1 (count (group-by pos? differences)))
      ;; 2. magnitute of the differences is between
      ;; 1-3
      (every? (fn [difference] (<= 1 (abs difference) 3))
              differences))))

(defn part-1 [s]
  (count (filter safe? (parse-inputs s))))

;; -----------------------------

(def level [1 3 2 4 5])
(safe? level)

(defn positions [pred xs])

;; -> idx
(defn first-unsafe-pair-idx
  [level]
  (reduce (fn [current-differences [idx difference]]
            (let [new-differences (conj current-differences
                                        difference)]
              (if-not (differences-safe? new-differences)
                (ensure-reduced idx)
                new-differences)))
    []
    (map-indexed vector
                 (map (fn [[a b]] (- a b))
                      (partition 2 1 level)))))

(comment
  (first-unsafe-pair-idx [1 3 2 4 5])
  (first-unsafe-pair-idx [1 1 2 4 5]))

(defn drop-position [v idx]
  (into
   (vec (take idx v))
   (drop (inc idx) v)))

(comment
  (drop-position [1 2 3] 0)
  (drop-position [1 2 3] 2)
  (drop-position [1 2 3] 1)
  (drop-position (into [] (range 10)) 1)
  [0 2 3 4 5 6 7 8 9]

  )

(def level [1 2 7 8 9])

(comment
  (let [first-unsafe-pair-idx
        (first-unsafe-pair-idx level)
        [x1 x2]
        [first-unsafe-pair-idx (inc first-unsafe-pair-idx)]
        ]
    [x1 x2]
    ;; (or (safe? (drop-position level x1))
    ;;     (safe? (drop-position level x2)))
    ))


(defn safe-with-dampener?
  [level]
  (boolean
   (or (safe? level)
       (let [first-unsafe-pair-idx (first-unsafe-pair-idx
                                    level)
             [x1 x2] [first-unsafe-pair-idx
                      (inc first-unsafe-pair-idx)]]
         (or (some (fn [x] (safe? (drop-position level x)))
                   (range (inc x2)))
             ;; didn't work, some edge case:
             ;; didn't investigate
             ;; (safe? (drop-position level x1))
             ;; (safe? (drop-position level x2))
             )))))

(comment
  (map safe-with-dampener?
       '([7 6 4 2 1]
         [1 2 7 8 9]
         [9 7 6 2 1]
         [1 3 2 4 5]
         [8 6 4 4 1]
         [1 3 6 7 9]))
  '(true
    false
    false
    true
    true
    true))

(defn part-2 [s]
  (count (filter safe-with-dampener? (parse-inputs s))))

(comment
  (safe-with-dampener?
   [20 6 4 2 1])

  (safe-with-dampener?
   [20 20 4 2 1])


  (map (juxt safe-with-dampener? identity)
       (take 10 (parse-inputs (slurp "inputs/2024/2/input"))))


  '([true [58 59 62 63 64 63]]
    [true [71 72 74 76 78 80 82 82]]
    [true [26 29 32 34 35 39]]
    [true [9 11 14 17 19 20 21 26]]
    [true [89 92 95 93 94 97 98]]
    [false [35 37 40 41 43 42 39]]
    [false [89 91 94 96 97 99 98 98]]
    [false [85 86 83 84 85 86 90]]
    [false [46 48 50 52 49 52 59]]
    [true [56 58 58 60 62]])

  (safe? [1 2 1 0])

  (safe-with-dampener? [10 11 9 8])
  (safe-with-dampener? [10 11 12 9 8])
  (safe-with-dampener? [10 11 12 9 8])

  (safe-with-dampener?
   [35 37 40 41 43 42 39])

  (differences [35 37 40 41 43 42 39])
  (differences [35 37 40 41 43 39])

  (safe? level)
  (safe? [1 2 7 8 9])
  (part-1 example-input)
  (part-1
   (slurp "inputs/2024/2/input"))
  402

  (time (part-2
         (slurp "inputs/2024/2/input")))
  455
  437
  452

  (map
   safe?
   '([7 6 4 2 1]
     [1 2 7 8 9]
     [9 7 6 2 1]
     [1 3 2 4 5]
     [8 6 4 4 1]
     [1 3 6 7 9]))

  '(true false false false false true)
  )
