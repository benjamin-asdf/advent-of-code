(ns day3 (:require [clojure.string :as str])
    )

(def example-input
  "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(defn make-int [bits-bools]
  (read-string
   (str "2r"
        (str/join
         (map {false "0" true "1"} bits-bools)))))


(let [report
      (->>
       ;; example-input
       (slurp "inputs/day3")
       str/split-lines
       (map
        (fn [line]
          (into [] (seq line)))))
      bits-length (count (first report))
      series-lengt (count report)
      vertical-series
      (into
       (for [i (range bits-length)]
         (into
          []
          (for [bits report]
            (bits i)))))
      gamma
      (into
       []
       (comp
        (map
         #(group-by identity %))
        (map
         (fn [bits]
           (<
            (count (bits \0))
            (/ series-lengt 2)))))
       vertical-series)
      epsilon  (map #(not %) gamma)

      gamma (make-int gamma)
      epsilon (make-int epsilon)]
  (* gamma epsilon))
;; 3895776

;; part2
;; after a good nights sleep and hammok time

(defn diagnostics [input]
  (->>
   input
   str/split-lines
   (map
    (fn
      [line]
      (into
       []
       (comp (map str)
             (map parse-long))
       (seq line))))))

(defn highest-identities [coll]
  (->>
   coll
   (group-by identity)
   vals
   (sort-by count)))

(defn oxygen-pred [nums]
  (let [sorted-nums (highest-identities nums)]
    (if (= 0 (apply - (map count sorted-nums)))
      1
      (first (last sorted-nums)))))

(defn scrubber-pred [nums]
  (let [sorted-nums (highest-identities nums)]
    (if (= 0 (apply - (map count sorted-nums)))
      0
      (ffirst sorted-nums))))

(defn
  select-bits*
  [pred {:keys [series]}]
  (let [first-bits (map ffirst series)
        bit (pred first-bits)
        new-series ((group-by ffirst series) bit)
        new-series
        (map (fn [[curr orig]]
               [(rest curr) orig]) new-series)]
    {:reduced
     (when-not (seq (rest new-series)))
     (last (first new-series))
     :series new-series}))

(defn
  select-bits
  [pred input]
  (some
   :reduced
   (iterate
    #(select-bits* pred %)
    {:series (map
              (juxt identity identity)
              (diagnostics input))})))

(defn bit-vec->int [v]
  (read-string (str "2r" (str/join (map str v)))))

(let [input (slurp "inputs/day3")
      scrubb (select-bits scrubber-pred input)
      oxy (select-bits oxygen-pred input)]
  (* (bit-vec->int scrubb) (bit-vec->int oxy)))

;; 7928162

(comment
  (some
   :reduced
   (take
    10
    (iterate
     #(select-bits* oxygen-pred %)
     {:series
      (map (juxt identity identity) (diagnostics example-input))})))
  (def
    scrubb-bits
    (select-bits
     scrubber-pred
     example-input))

  (bit-vec->int scrubb-bits)

  ;; whatever
  ;; (->
  ;;  (reduce
  ;;   (fn [[acc i] n]
  ;;     [(+ acc
  ;;         (bit-shift-left 1 n))
  ;;      (inc i)])
  ;;   [0 0]
  ;;   (reverse scrubb-bits))
  ;;  first)


  )
