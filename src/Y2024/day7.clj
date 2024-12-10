(ns Y2024.day7
  (:require [clojure.string :as str]
            [clojure.core.async :as a]
            [clojure.core.logic.pldb :as pldb]
            [clojure.core.logic.fd :as fd]
            [clojure.core.logic :as logic]))



;; now hyperon

(require-python '[hyperon :as hyp :refer [E S V G]])

(def metta (hyp/MeTTa))

(defn metta-run! [s] (py.. metta (run s)))

(defn metta-parse-single [s]
  (py.. metta
    (parse_single s)))

(defn metta-eval-expr [expr]
  (py.. metta (evaluate_atom expr)))

(defn metta-value-obj-value
  [o]
  (py.. o get_object -value))


;; I'll need to figure out a lot of MeTTa for this....

(def example-input
  "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")


(defn parse-input [s]
  (into
   [] (map
       (comp
        vec
        (partial map parse-long)
        #(re-seq #"\d+" %1))
       (clojure.string/split-lines s))))

(parse-input example-input)

[190 10 19]
;; a possible equation...

;; idea 1:
;; metta script should say
;; 'yes'

;; (-> equation Bool)
;; (equation-possible?)

;; idea 2:
;; 2a. build a knowledge base, like

;; (equation inputs outcome )
;; (equation (10 19) 190)

;; 2b. ?
;; - query all equations,
;; - filter 'possible'
;;


;; --------------------------

;; either way, one thing that is already
;; cool with metta would be the 'operator' like this:

;; I write that in the metta script

;; ok so this is my subsymbolic
;; implementation of concat op


(do

  (py.. metta
    (register_atom
     "concat-op"
     (hyp/OperationAtom
      "concat-op"
      (fn [a b]
        (parse-long (str a b))))))

  (metta-run! (slurp
               "/home/benj/repos/advent-of-code/day7.metta"))

  )




;; ok let's be somewhat naive,
;; go with something like idea 1
(def equation [190 10 19])

;; TODO:  I would need to call it 1 and 2 for part 1 and 2
(def m-operator (metta-parse-single "(operator)"))

(defn equation-possible-metta
  [equation m-operator]
  (let [[_ & inputs] equation]
    (reduce (fn [acc n]
              (E m-operator acc (hyp/ValueAtom n)))
      (hyp/ValueAtom (first inputs))
      (rest inputs))))

(defn part-1
  [equations]
  (let [keep-working-equation (metta-parse-single
                                "keep-working-equation")]
    (apply +
      (map metta-value-obj-value
        (keep first
              (map (fn [equation]
                     (metta-eval-expr
                       (E keep-working-equation
                          (hyp/ValueAtom (first equation))
                          (equation-possible-metta
                            equation
                            (metta-parse-single
                              "(operator-1)")))))
                equations))))))



(time
 (part-1
  (parse-input
   (slurp
    "/home/benj/repos/advent-of-code/inputs/2024/7/input"))))


;; part 2

(parse-input
 (slurp
  "/home/benj/repos/advent-of-code/inputs/2024/7/input"))


    ;; (def equation (first (parse-input example-input)))
    ;;
    ;; (run-equation 10 (19 10))
    ;;
