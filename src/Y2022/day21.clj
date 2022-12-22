(ns
    Y2022.day21
    (:require
     [clojure.string :as str]
     [clojure.core.async :as a]
     [clojure.core.logic.pldb
      :as
      pldb]
     [clojure.core.logic.fd :as fd]
     [clojure.core.logic :as logic]))


(let [o (Object.)]
  (defn log [msg] (println msg)))

(def
  input
  "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
")

(defn parse-monkeys [input]
  (into
   {}
   (comp
    (map
     (fn
       [[_
         name
         number
         monkey1
         op
         monkey2]]
       [(keyword name)
        (if
            number
            (parse-long number)
            [(resolve (read-string op))
             (keyword monkey1)
             (keyword monkey2)])])))
   (re-seq
    #"(\w+): (?:(\d+)|(?:(\w+) (.) (\w+)))"
    input)))

(defn ->monkey-job [job]
  {:monkey-job job})

(def monkey-job? :monkey-job)

(defn build-monkey-jobs [monkeys]
  (loop [monkey-lut monkeys]
    (if
        (some (complement monkey-job?) (vals monkey-lut))
        (recur
         (update-vals
          monkey-lut
          (fn [e]
            (cond
              (number? e)
              (let [c (a/promise-chan)]
                (a/>!! c e)
                (->monkey-job c))
              (vector? e)
              (let [[op a b] e
                    monkey-job-a (:monkey-job (monkey-lut a))
                    monkey-job-b (:monkey-job (monkey-lut b))]
                (if
                    (and monkey-job-a monkey-job-b)
                    (let [c (a/promise-chan)]
                      (if (boolean? op)
                        (log (str op e)))
                      (a/go
                        (let [v (op (a/<! monkey-job-a)
                                    (a/<! monkey-job-b))]
                          (a/>! c v)))
                      (->monkey-job c))
                    e))
              :else e))))
        monkey-lut)))

(defn part-1
  [input]
  (->>
   (parse-monkeys input)
   build-monkey-jobs
   :root
   :monkey-job
   a/<!!))

(part-1 (slurp "2022day21"))

104272990112064
;; part 2
;; maybe it runs quick enough so I can try every number until root says yes

(defn part-2 [input]
  (let [monkeys (parse-monkeys input)
        monkeys
        (assoc-in monkeys [:root 0] =)]
    (loop [batches (partition 10000 (range))]
      (log (ffirst batches))
      (or
       (first
        (pmap
         (fn [batch]
           (time
            (first
             (keep
              (fn [my-number]
                (let [monkey-lut
                      (build-monkey-jobs
                       (assoc monkeys :humn my-number))]
                  (when
                      (->
                       monkey-lut
                       :root
                       :monkey-job
                       a/<!!)
                      my-number)))
              batch))))
         (take 12 batches)))
       ;; (recur (drop 12 batches))
       ))))

(part-2 input)
(def res (part-2 (slurp "2022day21")))

(take 2 (partition 10 (range)))
(take 2 (drop 2 (partition 10 (range))))


;; chemical computing
;; each monkey a mol
;; then they react
;; I need a 3 way react then



(defn monkey-business [monkey-lut]
  (reduce-kv
   (fn [m k monkey]
     (assoc m k
            (if (monkey-job? monkey)
              monkey
              (if
                  (number? monkey)
                  (->monkey-job monkey)
                  (let [[op a b] monkey
                        a (:monkey-job a)
                        b (:monkey-job b)]
                    (if (and a b)
                      (->monkey-job (op a b))
                      monkey))))))
   {}
   monkey-lut))

(first
 (keep
  (fn [m] (when (every? monkey-job? (vals m)) m))
  (iterate monkey-business (parse-monkeys input))))
