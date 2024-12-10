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

;; --------------------------------
;; the clojure code at
;; src/Y2021/day7.clj
;; does the equivalent of calling this:

;; (keep-working-equation 190 (operator 10 19))
;; where the second arg is just the tree of operators
;; (as expression-atom)
;; (operator a (operator b c)) ...





;; --------------------------------------------------------

(= (run-equation
    $outcome
    $a
    $inputs)
   (if (== $inputs ())
       (if (== $outcome $a)
           $outcome
           (empty))
       (if (< $outcome $a)
           (empty)
           (run-equation
            $outcome
            ((operator-1)
             $a
             (car-atom $inputs))
            (cdr-atom $inputs)))))

(= (part-1 $outcome $inputs)
   (run-equation
         $outcome
         (car-atom $inputs)
         (cdr-atom $inputs)))


;; --------------------------------
;; part-2

(= (run-equation-2
    $outcome
    $a
    $inputs)
   (if (== $inputs ())
       (if (== $outcome $a)
           $outcome
           (empty))
       (if (< $outcome $a)
           (empty)
           (run-equation-2
            $outcome
            ((operator)
             $a
             (car-atom $inputs))
            (cdr-atom $inputs)))))

(= (part-2 $outcome $inputs)
   (run-equation-2
         $outcome
         (car-atom $inputs)
         (cdr-atom $inputs)))

















