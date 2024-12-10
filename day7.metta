;; part1
(= (operator-1) +)
(= (operator-1) *)

;; part2
(= (operator) concat-op)
(= (operator) +)
(= (operator) *)

(= (keep-working-equation
    $input
    $equation-expr)
   (if (== $input $equation-expr)
       $input
       (empty)))



;; --------------------------------------------------------



;; (keep-working-equation 190 ($op 10 19))




;; and because it is relational:

;; !(let $op (operator)
;;    (keep-working-equation 190 ($op 10 19)))

;; [(outcome 190 operator *)]


;; ----------------------------------------
;; trying somehting:



;; lets say we put into the knowledge base:

;; (equation 190 (10 19))
;; (equation 3267 (81 40 27))
;; (equation 83 (17 5))

;; (= (operator-part-1) +)
;; (= (operator-part-1) *)

;; this has  bug, doesn't make all possible operators: 
;; (= (calc-equation $inputs)
;;    (let* (($head (car-atom $inputs))
;;           ($tail (cdr-atom $inputs)))
;;      (if (== $tail ())
;;          $head
;;          ((operator-part-1)
;;           (car-atom $inputs)
;;           (calc-equation
;;            (cdr-atom $inputs))))))

;; !(calc-equation (1 2 3))

;; [5, 6, 6, 7]

;; !(* (* 1 2) 3)

;; ah this one is missing,what
;; !(* (+ 1 2) 3)


;; !(match &self
;;    (equation $res $inputs)
;;    (if
;;     (== $res (calc-equation $inputs))
;;     $res
;;     (empty)))


;; this would work and do the optimization with the earlier out I think

(=
 (run-equation $outcome $a $b)
 (if
  (== $b ())
  $a
  (if
   (< $outcome $a)
   (empty)
   (run-equation
    $outcome
    ((operator) $a (car-atom $b))
    (cdr-atom $b)))))

(=
 (run-equation $outcome $inputs)
 (run-equation
  $outcome
  (car-atom $inputs)
  (cdr-atom $inputs)))

(= (keep-equation $outcome $inputs)
   (if
    (== (run-equation $outcome $inputs) $outcome)
    $outcome
    (empty)))


;; !(run-equation 190 (19 10))
;; !(run-equation 10 (19 10))












