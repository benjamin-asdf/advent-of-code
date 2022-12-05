(ns Y2022.day5
  (:require [clojure.string :as str]))


(def
  input
  "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn parse [input]
  (let [[crates-str moves-str]
        (str/split input #"\n\n")
        [longest :as crate-lists]
        (->>
         (str/split-lines
          crates-str)
         drop-last
         (sequence
          (comp
           (map
            (fn [line]
              (->> (re-seq #"(    )|\[(\w)\]" line)
                   (map (fn [[_ _ crate]] crate)))))))
         reverse)
        crates
        (->>
         crate-lists
         (map
          (fn [lst]
            (take
             (count longest)
             (concat lst (repeat nil)))))
         (apply map vector)
        (into [] (map #(into [] (remove nil? %)))))
        moves
        (->>
         (re-seq #"move (\d+) from (\d+) to (\d+)" moves-str)
         (mapcat
          (fn [[_ amount from to]]
            (repeat (parse-long amount)
                    [(dec (parse-long from))
                     (dec (parse-long to))]))))]
    {:crates crates
     :moves moves
     :moves-part-2
     (->>
         (re-seq #"move (\d+) from (\d+) to (\d+)" moves-str)
         (map
          (fn [[_ amount from to]]
            {:to (dec (parse-long to))
             :amount (parse-long amount)
             :from (dec (parse-long from))})))}))

(defn step-crate [crates [from to]]
  (-> crates
      (update from pop)
      (update to conj (peek (crates from)))))

(defn part-1 [input]
  (let [{:keys [crates moves]} (parse input)]
    (->> (reduce step-crate crates moves)
         (map peek)
         str/join)))

(part-1 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day5"))
"PSNRGBTFT"

(def crates [["Z" "N"] ["M" "C" "D"] ["P"]])
(def moves
  '([1 0] [0 2] [0 2] [0 2] [1 0] [1 0] [0 1]))

(comment (step-crate crates (first moves))
         (-> crates
             (update 1 pop)
             (update 0 conj (peek (crates 1))))
         [["Z" "N" "D"] ["M" "C"] ["P"]])

(defn step-crate-part-2 [crates {:keys [from to amount]}]
  (-> crates
      (update from #(drop-last amount %))
      (update to concat (take-last amount (crates from)))))

(defn part-2 [input]
  (let [{:keys [crates moves-part-2]} (parse input)]
    (->> (reduce step-crate-part-2 crates moves-part-2)
         (map last)
         str/join)))

(part-2 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day5"))
"BNTZFPMMW"
