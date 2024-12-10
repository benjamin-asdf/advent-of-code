(ns Y2024.day7
  (:require [libpython-clj2.require :refer [require-python]]
            [libpython-clj2.python :refer [py. py..] :as
             py]))


;; hyperon interop
;; ---------------------------------------------------
(require-python '[hyperon :as hyp :refer [E S V G]])

(do
  (def metta (hyp/MeTTa))
  (py.. metta
    (register_atom "concat-op"
                   (hyp/OperationAtom
                    "concat-op"
                    (fn [a b]
                      (parse-long (str a b))))))
  (metta-run!
   (slurp "/home/benj/repos/advent-of-code/day7.metta")))


(defn metta-run! [s] (py.. metta (run s)))

(defn metta-parse-single [s]
  (py.. metta
    (parse_single s)))

(defn metta-eval-expr [expr]
  (py.. metta (evaluate_atom expr)))

(defn metta-value-obj-value
  [o]
  (py.. o get_object -value))

;; ---------------------------------

;; https://adventofcode.com/2024/day/7
(def example-input "")

(defn parse-input [s]
  (into
   [] (map
       (comp
        vec
        (partial map parse-long)
        #(re-seq #"\d+" %1))
       (clojure.string/split-lines s))))


;; build an expression like
;;
;; (operator a (operator b c))
(defn equation-possible-metta
  [equation m-operator]
  (let [[_ & inputs] equation]
    (reduce (fn [acc n]
              (E m-operator acc (hyp/ValueAtom n)))
            (hyp/ValueAtom (first inputs))
            (rest inputs))))

;; just book keeping / setup,
;; in effect metta:
;; !(keep-working-equation outcome expr)
;; where expr is (operator a (operator b ...))
;; for all inputs
(defn run
  [equations m-operator]
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
                                  m-operator))))
                           equations))))))


(defn part-1
  [equations]
  (run equations (metta-parse-single "(operator-1)")))

(defn part-2
  [equations]
  (run equations (metta-parse-single "(operator)")))

;; correct, but not fast enough.
;; So I got inspired by it and made day7_core_logic.clj

(time (part-1 (parse-input example-input)))
3749
(time (part-2 (parse-input example-input)))
11387















;; -----------------------------------
;; second version


(defn run-part
  [part-op input]
  (reduce +
          (map metta-value-obj-value
               (keep ffirst
                     (map metta-run!
                          (map (fn [[outcome & inputs]]
                                 (format "!(%s %s %s)"
                                         part-op
                                         outcome
                                         (reverse (into (list)
                                                        inputs))))
                               (parse-input example-input)))))))


(comment
  (run-part "part-1" (parse-input example-input))
  3749
  (run-part "part-2" (parse-input example-input))
  11387)

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
