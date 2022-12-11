(ns Y2022.day11
  (:require
   [clojure.math :as math]))

(def input
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defn monkey-table [input]
  {:update-worry-fns
   (into []
         (map
          (fn
            [[_ old op arg]]
            (eval `(fn [~'old] ~(read-string (str "(" op " " old " " arg ")"))))))
         (re-seq
          #"new = (old) ([\*\+]) ((?:old)|(?:\d+))"
          input))
   :test-fns (into []
                   (map
                    (fn
                      [[_ y]]
                      (fn
                        [x]
                        (zero? (mod x (parse-long y))))))
                   (re-seq
                    #"divisible by (\d+)"
                    input))
   :targets (into [] (map
              #(into [] %)
              (partition
               2
               (sequence
                (comp
                 (map second)
                 (map parse-long))
                (re-seq #"throw to monkey (\d+)" input)))))})

(defn get-monkeys [input]
  (into [] (map (fn [[_ items-str]] (read-string (str "[" items-str "]")))
        (re-seq #"Starting items: (.+?)\n" input))))

;; you could inc a counter when it inspects
;; or expand the round into insepctions etc
;;

(defn next-circ [length n]
  (mod (+ n 1) length))

(defn part-1 [input]
  (let [round-count 20
        table (monkey-table input)
        monkeys-input (get-monkeys input)
        next (partial next-circ (count monkeys-input))
        play-1
        (fn [{:keys [monkeys monkey round-n monkey-insepction-count]
              :as state}]
          (let [items (nth monkeys monkey)]
            (if (seq items)
              (let [item (first items)
                    item ((get-in table [:update-worry-fns monkey]) item)
                    item (quot item 3)
                    to-monkey
                    (if ((get-in table [:test-fns monkey]) item)
                      (get-in table [:targets monkey 0])
                      (get-in table [:targets monkey 1]))
                    new-monkeys
                    (-> monkeys
                        (update to-monkey conj item)
                        (update monkey #(subvec % 1)))]
                (-> state
                    (assoc :monkeys new-monkeys)
                    (update-in [:monkey-insepction-count monkey] (fnil inc 0))))
              (let [next-monkey (next monkey)]
                (-> state
                    (assoc :monkey next-monkey)
                    (update :round-n (if (zero? next-monkey) inc identity)))))))]
    (->>
     (iterate play-1 {:monkeys monkeys-input :round-n 0 :monkey 0})
     ;; (take-while (comp #(< % 1) :round-n))
     (take-while (comp #(< % round-count) :round-n))
     last
     :monkey-insepction-count
     vals
     (sort-by -)
     (take 2)
     (apply *))))

(part-1 (slurp "inputs/2022/day11"))

88208
