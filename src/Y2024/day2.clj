(ns Y2024.day2)

;;

(def example-input
  "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9")

(defn parse-inputs
  [s]
  (map read-string
    (map (fn [line] (str "[" line "]"))
         (clojure.string/split s #"\n"))))

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

(defn safe? [level] (differences-safe? (differences level)))

(defn part-1 [s]
  (count (filter safe? (parse-inputs s))))

;; -----------------------------

;; -> idx
(defn first-unsafe-pair-idx[level]
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


(defn drop-position [v idx]
  (into
   (vec (take idx v))
   (drop (inc idx) v)))

(defn safe-with-dampener?
  [level]
  (boolean
   (or (safe? level)
       (let [first-unsafe-pair-idx (first-unsafe-pair-idx
                                    level)
             [x1 x2] [first-unsafe-pair-idx (inc first-unsafe-pair-idx)]]
         (or (some (fn [x] (safe? (drop-position level x)))
                   (range (inc x2)))
             ;; didn't work, some edge case:
             ;; didn't investigate
             ;; (safe? (drop-position level x1))
             ;; (safe? (drop-position level x2))
             )))))

(defn part-2 [s]
  (count (filter safe-with-dampener? (parse-inputs s))))
