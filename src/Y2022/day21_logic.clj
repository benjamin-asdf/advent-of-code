(ns
    Y2022.day21.logic
    (:require
     [clojure.string :as str]
     [clojure.core.async :as a]
     [clojure.core.logic.pldb
      :as
      pldb]
     [clojure.core.logic.fd :as fd]
     [clojure.core.logic :as logic]))

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
            [op (keyword monkey1) (keyword monkey2)])])))
   (re-seq
    #"(\w+): (?:(\d+)|(?:(\w+) (.) (\w+)))"
    input)))

(defn bind [monkey->lvar [monkey-k monkey]]
  (let [var (monkey->lvar monkey-k)]
    (cond
      ;; human
      (= var monkey)
      logic/succeed
      (number? monkey)
      (logic/== var monkey)
      :else
      (let [[op a b] monkey
            a (monkey->lvar a)
            b (monkey->lvar b)]
        (case op
          "=" (logic/== a b)
          ((case op
             "+" fd/+
             "-" fd/-
             "*" fd/*
             "/" fd/quot)
           a b var))))))

(defn bind-all [monkey->lvar monkeys]
  (logic/and* (map (partial bind monkey->lvar) monkeys)))

;; part 1
(let [monkeys (parse-monkeys input)
      monkey->lvar (update-vals monkeys (fn [_] (logic/lvar)))]
  (logic/run 1 [q]
    (logic/== (:root monkey->lvar) q)
    (bind-all monkey->lvar monkeys)))

;; part 2
(defn part-2 [input]
  (let [monkeys (parse-monkeys input)
        monkey->lvar (update-vals monkeys (fn [_] (logic/lvar)))
        monkeys
        (->
         monkeys
         (assoc :humn (:humn monkey->lvar))
         (assoc-in [:root 0] "="))]
    (logic/run 1 [q]
      (logic/== q (:humn monkey->lvar))
      (bind-all monkey->lvar monkeys))))

(part-2 (slurp "2022day21"))
(3220993874133)
