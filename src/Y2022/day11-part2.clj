(ns Y2022.day11
  (:require
   [clojure.math :as math]))

(set! *warn-on-reflection* true)

(defn divisible-by [^clojure.lang.BigInt n ^clojure.lang.BigInt x]
  (zero? (mod x n)))

(defn monkey-table [input]
  (let [divisibles
        (map bigint (map parse-long (map second (re-seq #"divisible by (\d+)" input))))]
    {:divisibles divisibles
     :update-worry-fns
     (into []
           (map
            (fn
              [[_ old op arg]]
              (eval `(fn ^clojure.lang.BigInt [^clojure.lang.BigInt ~'old] ~(read-string (str "(" op " " old " " arg ")"))))))
           (re-seq
            #"new = (old) ([\*\+]) ((?:old)|(?:\d+))"
            input))
     :test-fns
     (into [] (map (fn [n] (fn [x] (divisible-by n x)))) divisibles)
     :targets (into [] (map
                        #(into [] %)
                        (partition
                         2
                         (sequence
                          (comp
                           (map second)
                           (map parse-long))
                          (re-seq #"throw to monkey (\d+)" input)))))}))

(defn get-monkeys [input]
  (into [] (map (fn [[_ items-str]] (read-string (str "[" items-str "]")))
        (re-seq #"Starting items: (.+?)\n" input))))

(defn next-circ [length n]
  (mod (+ n 1) length))

(defn part-2 [input round-count]
  (let
      [table (monkey-table input)
       only-care-divisble (apply * (:divisibles table))
       monkeys-input (into [] (map #(into [] (map bigint) %)) (get-monkeys input))
       next (partial next-circ (count monkeys-input))
       target (memoize (fn [monkey n] (get-in table [:targets monkey n])))
       play-1
       (fn [{:keys [monkeys monkey round-n monkey-insepction-count] :as state}]
         (let [items (nth monkeys monkey)]
           (if (seq items)
             (let [^clojure.lang.BigInt item (first items)
                   item ((nth (-> table :update-worry-fns) monkey) item)
                   test-fn
                   (nth (-> table :test-fns) monkey)
                   to-monkey
                   (if (test-fn item)
                     (target monkey 0)
                     (target monkey 1))
                   ;; ok I half randomly tried (mod item (* 23 17...)) and it started working
                   item (mod item only-care-divisble)
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
       (take-while (comp #(< % round-count) :round-n))
       last
       :monkey-insepction-count
       vals
       (sort-by -)
       (take 2)
       (apply *))))

(time (part-2 (slurp "/home/benj/repos/clojure/aoc/inputs/2022/day11") 10000))
21115867968
"Elapsed time: 1091.237098 msecs"
