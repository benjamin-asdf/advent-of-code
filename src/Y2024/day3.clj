(ns Y2024.day3)

;; ----------------------------------------------------
;; There is a ASMR live code of this uploaded
;;
;; ----------------------------------------------------



;; input: corrupted memory


;; mul(44,46) -> 'multiply 2 numbers'
;; 1-3 digit numbers
;;
;; ignore invalid characters
;; mul ( 2 , 4 )


;; lol, let's regex this up I guess

(comment
  (def example-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

  ;; counting more than 3 digits as invalid
  (re-seq #"mul\(\d{1,3}," "mul(4444,2)")


  ;; now the capture groups
  (map
   (fn [[_ ]])
   (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" "mul(444,2)"))

  (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" "mul(444,2)")
  ;; first is the match,
  ;; then group 1, 2, ...

  ((fn [[_ a b]] (* (parse-long a) (parse-long b))) ["mul(444,2)" "444" "2"])
  888

  ;; for example...

  ;; 1. extract all valid mul(xxx,xxx)
  (def input example-input))


(defn part-1
  [input]
  ;; + all valid mul substrings.
  (transduce (map (fn [[_ a b]]
                    (* (parse-long a) (parse-long b))))
             +
             (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" input))
  ;; 2. mul the inputs
  ;; 3. sum the results
  )

;; ------------------------------------------------

;; -------- part 2 ----------------

;;
;; the program is now a machine
;;
;; state: is evaluating, mul enabled

;; transitions:
;; do()   : enabled -> disabled
;; dont'(): disabled -> enabled
;;

(def example-input-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

;; so first get a new regex that also does the other 2 cases
(comment
  (re-seq #"do\(\)" "do()"))

(defn parse-part-2
  [input]
  (re-seq
   ;;
   #"(mul\((\d{1,3}),(\d{1,3})\))|(do\(\))|(don't\(\))"
   input))

;; 1. extract all instructions
;; instructions: do, don't, mul


   ;;
   ;; 2. evaluate instructions:
   ;; - do: enable mul
   ;; - don't: disable mul
   ;; - mul: if enabled, then mul, else nothing
   ;;            (or 0)
(defn part-2-evaluate
  [parsed-input]
  (:acc
    (reduce
      ;; for each instruction...
      ;; accumulate the result
      ;;
      (fn [state
           [ ;; 0. the match
            _
            ;; 1. the mul match
            mul?
            ;; the numbers, if present
            mul-a mul-b
            ;;
            do?
            ;;
            dont?]]
        ;; -----------------------------
        ;;  evaluator
        ;; -----------------------------
        (cond-> state
          (and (:enabled? state) mul?)
          (update
           :acc
           +
           (* (parse-long mul-a)
              (parse-long mul-b)))
          do? (assoc :enabled? true)
          dont? (assoc :enabled? false)))
      {;; starts true
       :enabled? true
       ;; the mul outcome
       :acc 0}
      parsed-input)))







;; Rich comment:

(comment
  (part-1
   (slurp "/home/benj/repos/advent-of-code/inputs/2024/3/input"))

  (part-2-evaluate
   (parse-part-2 (slurp "/home/benj/repos/advent-of-code/inputs/2024/3/input")))


  ;; 👇
  (part-2-evaluate
   '(
     ["mul(2,4)" "mul(2,4)" "2" "4" nil nil]
     ["don't()" nil nil nil nil "don't()"]
     ["mul(5,5)" "mul(5,5)" "5" "5" nil nil]
     ["mul(11,8)" "mul(11,8)" "11" "8" nil nil]
     ["do()" nil nil nil "do()" nil]
     ["mul(8,5)" "mul(8,5)" "8" "5" nil nil]))
  48
  ;; alright
  ;; I'm excited.


  ;; I think the emacs regex doesn't do \d?

  ;; the syntax:
  ;;
  ;; {min, max}
  ;; in case you did not know.

  ;; so:

  ;; matches 3 (max)
  (re-seq #"\d{1,3}," "1234,")


  )
