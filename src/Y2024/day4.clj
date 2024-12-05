(ns Y2024.day4
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [libpython-clj2.require :refer [require-python]]
            [libpython-clj2.python :refer [py. py..] :as
             py]))


;; ---------------------
;; 🎅🎅🎅 ho, ho, ho!
;;
;; 🍵 -  ✅
;;  tea, check, keyboard, check, let's go elves adventure!
;; --------------------------------------------------

(def target-word "XMAS")

;; ---------------------------------------------

(do
  (require-python '[numpy :as np])
  (require-python '[torch :as torchj])
  (require-python '[torch.nn.functional :as F]))

;; Trying to cook a Cellular Automaton:
;; -----------------------------------

(defn ->grid
  [input]
  (into [] (map vec (str/split-lines input))))

(defn play-ca
  "Returns a new board with CA weights applied.


  Lit:

  https://pytorch.org/docs/stable/generated/torch.nn.functional.conv2d.html#torch.nn.functional.conv2d
  https://github.com/tomgrek/gameoflife

  "
  [weights board]
  (let [filters (py.. weights
                  (to :dtype torch/float)
                  (view 1 1 3 3))
        inputs (py.. board
                 (to :dtype torch/float)
                 (view 1
                       1
                       (py.. board (size 0))
                       (py.. board (size 1))))]
    (py.. (F/conv2d inputs filters :padding 1)
      (view (py.. board (size 0))
            (py.. board (size 1))))))

(defn ->weights [rule]
  (torch/tensor rule :dtype torch/float))

(def rules
  [
   [[0 0 0]
    [0 0 0]
    [1 0 0]]

   [[0 0 0]
    [0 0 0]
    [0 1 0]]

   [[0 0 0]
    [0 0 0]
    [0 0 1]]

   [[0 0 0]
    [0 0 1]
    [0 0 0]]

   [[0 0 1]
    [0 0 0]
    [0 0 0]]

   [[0 1 0]
    [0 0 0]
    [0 0 0]]

   [[1 0 0]
    [0 0 0]
    [0 0 0]]

   [[0 0 0]
    [1 0 0]
    [0 0 0]]])

;; ---------------------------------
;; Follow the trail of XMAS, following a single rule

;; X -> M -> A -> S
;; 1    2    3    4
;;
;; ------------------------------------------

;; There are probably a way smarter ways of using CA to solve this.
;; but this is what I came up with in my naivitee:
;;
;; While there are letters left,
;; 1. board = bitwise and with the board and mask
;;    for the next letter (first is X)
;;
;;    Saying, give me a board where only the M's that follow X's survive
;;    and so forth.
;;
;; 2. board = run the rule
;;
;; output = count elements in remaining board
;;

;; learning from part 2
;; I would want to write something like:
;;
#_[[\X \M \A \S]]
#_
[[\X 0 0 0]
 [0 \M 0 0]
 [0 0 \A 0]
 [0 0 0 \s]]


(defn mask-for-letter
  [grid letter]
  (torch/tensor (vec (map (fn [row]
                            (vec (map #(get {letter 1} % 0)
                                      row)))
                          grid))
                :dtype
                torch/float))

;;
(defn play-rule
  [rule grid target-word]
  (reduce
   (fn [board letter]
     (let [next-board (play-ca (->weights rule) board)
           next-letter-mask (mask-for-letter grid letter)
           next-board (torch/bitwise_and
                       (py.. next-board
                         (to :dtype torch/int8))
                       (py.. next-letter-mask
                         (to :dtype torch/int8)))]
       next-board))
   (mask-for-letter grid (first target-word))
   (rest target-word)))

(defn count-xmas
  [rule grid target-word]
  (py.. (torch/nonzero (py..
                         (play-rule rule grid target-word)
                         (view -1)))
    (numel)))

(defn part-1
  [input]
  (let [grid (->grid input)]
    (apply +
      (map (fn [rule] (count-xmas rule grid target-word))
        rules))))


;;
;; ok use prime numbers:
;;
;; A - 2
;; M - 3
;; S - 5
;; everything else - 0

(def mas-map
  {\A 2 \M 3 \S 5})

(def MAS-rules
  (walk/postwalk-replace
   mas-map
   ;; Being able to write something like this as program
   ;; was the vision
   [[[\M 0 \S]
     [0 \A 0]
     [\M 0 \S]]

    [[\S 0 \S]
     [0 \A 0]
     [\M 0 \M]]

    [[\M 0 \M]
     [0 \A 0]
     [\S 0 \S]]

    [[\S 0 \M]
     [0 \A 0]
     [\S 0 \M]]]))

;; the number for an A in the middle, applying the rules
;; This is complected with the above rules and there must be a way to say such thing
;; (I was just trying to cook CA from first principles)

;; 72
(def xmas-target
  (apply + (map #(* % %) (map (comp mas-map first) ["M" "M" "A" "S" "S"]))))

;; makes a board
(defn MAS-mask
  [grid]
  (torch/tensor
   (mapv
    (partial mapv #(mas-map % 0))
    grid)
   :dtype
   torch/float))

(defn count-nonzero
  [t]
  (torch/numel (torch/nonzero (py.. t (view -1)))))

(defn xmasses
  [rule board]
  (torch/eq (play-ca (->weights rule) board) xmas-target))

(defn part-2
  [input]
  (count-nonzero (let [grid (->grid input)
                       ;; making this board is the bottleneck, it's ~7ms
                       board (MAS-mask grid)]
                   (torch/sum (torch/stack
                                (vec (map (fn [rule]
                                            (xmasses rule
                                                     board))
                                       MAS-rules)))
                              :dim 0
                              :keepdim true))))

(part-2 ".M.S......\n..A..MSMS.\n.M.S.MAA..\n..A.ASMSM.\n.M.S.M....\n..........\nS.S.S.S.S.\n.A.A.A.A..\nM.M.M.M.M.\n..........\n")
9

;; 🎉 🎉🎉🎉🎉
;;

(comment

  (do (require-python '[torch.cuda :as torch.cuda])
      (torch/set_default_device "cpu")
      (time (part-2
             (slurp "/home/benj/repos/advent-of-code/inputs/2024/4/input"))))

  ;; CPU:

  ;; "Elapsed time: 9.921373 msecs"
  ;; "Elapsed time: 10.219452 msecs"
  ;; "Elapsed time: 8.859742 msecs"
  ;; "Elapsed time: 8.369945 msecs"
  ;; "Elapsed time: 8.44238 msecs"
  ;; "Elapsed time: 8.561093 msecs"
  ;; "Elapsed time: 10.026981 msecs"
  ;; "Elapsed time: 9.326467 msecs"
  ;; "Elapsed time: 8.731832 msecs"
  ;; "Elapsed time: 8.859693 msecs"

  (do (require-python '[torch.cuda :as torch.cuda])
      (torch/set_default_device "cuda")

      ;; GPU:
      (time (part-2
             (slurp "/home/benj/repos/advent-of-code/inputs/2024/4/input"))))

  ;; "Elapsed time: 25.04139 msecs"
  ;; "Elapsed time: 10.402461 msecs"
  ;; "Elapsed time: 10.632494 msecs"
  ;; "Elapsed time: 9.809857 msecs"
  ;; "Elapsed time: 9.334082 msecs"
  ;; "Elapsed time: 10.786924 msecs"
  ;; "Elapsed time: 10.488293 msecs"
  ;; "Elapsed time: 8.846165 msecs"
  ;; "Elapsed time: 8.565997 msecs"
  ;; "Elapsed time: 10.686495 msecs"

  ;; probably dominated by data transfers
  ;; and making the board is the bottleneck anyway


  )
