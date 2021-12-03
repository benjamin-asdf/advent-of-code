(ns
    increases
    "day 1"
    (:require
     [clojure.string :as str]))

(def nums
  (map
   parse-long
   (str/split-lines (slurp "input"))))

(defn
  get-increases
  [nums]
  (::acc
   (reduce
    (fn
      [{:keys [::acc ::last-i]} i]
      (let [acc (if-not
                    last-i
                    0
                    (if (> i last-i) (inc acc) acc))]
        {::acc acc ::last-i i}))
    nil
    nums)))

(defn partition-sliding
  [n sx]
  (when (>= (count sx) 3)
    (cons (take 3 sx) (partition-sliding n (rest sx)))))

(->>
 nums
 (partition-sliding 3)
 (map (partial reduce +))
 get-increases)


;; after learning about partition

;; 1)
(defn get-increases
  [nums]
  (->>
   nums
   (partition 2 1)
   (filter (fn [[a b]] (> b a)))
   count))

;; 2)
(->>
 nums
 (partition 3 1)
 (map (partial reduce +))
 get-increases)

(comment
  (partition 2 1 (take 10 nums))
  (partition 3 1 (take 10 (range)))
  (partition-sliding
   3
   (into [] (range 10)))
  (get-increases [0])
  (get-increases [1 1 1 1])
  (get-increases [1 1 1 0])
  (get-increases [1 1 1 10])
  (get-increases nums)
  ;; => 1557
  )
