(ns Y2022.day21
  (:require [clojure.string :as str]
            [clojure.core.async :as a]))

(defn monkey-yell [jobs name]
  (let [job (jobs name)]
    (if (number? job)
      job
      (let [[op a b] job]
        (case op
          :+ (+ (monkey-yell jobs a) (monkey-yell jobs b))
          :- (- (monkey-yell jobs a) (monkey-yell jobs b))
          :* (* (monkey-yell jobs a) (monkey-yell jobs b))
          :/ (/ (monkey-yell jobs a) (monkey-yell jobs b)))))))


(def input
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
drzm: hmdt ")

(defn ->monkey-jobs [monkeys [monkey job]]
  (assoc
   monkeys
   monkey
   (a/go
     (if (number? job)
       job
       (let [[op a b] job]

         ;; I have a thinko
         ;; monkeys at this point are not channels
         ;; I need another way to ref other monkeys

         (op (a/<! (monkeys a))
             (a/<! (monkeys b)))))

     ))
  )

(->>
 (into {}
       (comp
        (map (fn [[_ name number monkey1 op monkey2]]
               [(keyword name)
                (if number
                  (parse-long number)
                  [(resolve (read-string op))
                   (keyword monkey1)
                   (keyword monkey2)])])))
       (re-seq
        #"(\w+): (?:(\d+)|(?:(\w+) (.) (\w+)))"
        input))
 ;; (monkey-yell :root)
 (reduce ->monkey-jobs {})
 )
