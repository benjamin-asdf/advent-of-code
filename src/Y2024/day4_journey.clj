(ns Y2024.day4
  (:require [clojure.string :as str]
            [libpython-clj2.require :refer [require-python]]
            [libpython-clj2.python :refer [py. py..] :as
             py]))

(do
  (require-python '[numpy :as np])
  (require-python '[torch :as torch])
  (require-python '[torch.nn.functional :as F]))

;; ---------------------
;; 🎅🎅🎅 ho, ho, ho!
;;
;; 🍵 -  ✅
;;  tea, check, keyboard, check, let's go elves adventure!
;; --------------------------------------------------


;; input:  word search
;; taret word: XMAS
(def target-word "XMAS")

(def example-input
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(def input example-input)

(defn ->grid
  [input]
  (into [] (map vec (str/split-lines input))))



;; I actually slept a night over it.
;; deciced to try to do it with a CA
;;


;; let's get libpython-clj


;; trying to do the one that goes
;;
;;
;;      +---+       +---+
;;      | --+------>|   |
;;      +---+       +---+
;;
;; to the right
;;


(def weights
  (torch/tensor [[0 0  0]
                 [0 10 1]
                 [0 0  0]]))

(def board
  (torch/tensor [[0 0 0]
                 [0 1 0]
                 [0 0 0]]))



;; wonderful.
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

;; I need to find the 'trail' of XMAS
;; that follows a single rule


;; X -> M -> A -> S
;; 1    2    3    4
;;

;; I'm thinking something like first
;;
;; for each rule,
;;
;; While there are letters left,
;; 1. mask = bitwise and with the board and mask for the next letter
;;    (first is X)
;;    -> 2.
;; 2. run the rule
;;
;; output = nonzero(mask)
;;
;; I think this makes sense.
;; I think it should count the 'trails' for XMAS,
;; following one of the rules.
;;

(defn mask-for-letter-1
  [grid letter]
  (vec (map (fn [row] (vec (map #(get {letter 1} % 0) row)))
            grid)))

(defn mask-for-letter
  [grid letter]
  (torch/tensor (mask-for-letter-1 grid letter) :dtype torch/float))

(comment
  (def letter \X)
  (mask-for-letter (->grid example-input) (first "XMAS"))
  (mask-for-letter (->grid example-input) (second "XMAS")))
;; yes, something like that.

(def rule [[0 0 0] [1 0 0] [0 0 0]])
(defn ->weights [rule]
  (torch/tensor rule :dtype torch/float))

;; ok, lets play with this simplest rule, go right

;; we should in the end find 3
;;

(def grid (->grid example-input))
(def mask (mask-for-letter grid (first "XMAS")))

(comment [mask (play-ca (->weights rule) mask)])
;; that seems correct.

;; now the outcome bitwise and with the next letter mask

(def mask-2 (play-ca (->weights rule) mask))

;; wait, not bitwise and I.g.
;; or cast it

(torch/bitwise_and
 (py..
     (mask-for-letter grid (second "XMAS"))
   (to :dtype torch/int8))
 (py.. mask-2 (to :dtype torch/int8)))

(defn play-rule
  [rule grid target-word]
  (reduce
    (fn [board letter]
      ;;
      ;; While there are letters left,
      ;; 1. board = bitwise and with the board and mask
      ;; for the next letter
      ;;    (first is X)
      ;;    -> 2.
      ;; 2. run the rule
      ;;
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

(play-rule rule grid target-word)

;; 3!!
;; that is correct!

(let [grid (->grid example-input)]
  (apply + (map
            (fn [rule] (count-xmas rule grid target-word))
            rules)))
;; yes 18 now
;; -----------------------------

;; ok let's go

(defn part-1
  [input]
  (let [grid (->grid input)]
    (apply +
      (map (fn [rule] (count-xmas rule grid target-word))
        rules))))

(part-1
 (slurp "/home/benj/repos/advent-of-code/inputs/2024/4/input"))
2575

;; --------------------------------
;; ok now part 2

;; ok trying to built on my part1...


;; it is a CA
;; I need to say
;; 1. find the A's where there is 1 M input and 1 S input.
;;    <- and this is a CA
;;
;; 2. then bitwise and again
;; that is the A's with 2 of those inputs making an X
;;

(def grid (->grid "M.S\n.A.\nM.S\n"))

;; ok use prime numbers:
;;
;; A - 2
;; M - 3
;; S - 5
;; everything else - 0

(def mas-map
  {\A 2 \M 3 \S 5})

(def MAS-rules
  [
   ;; and then the X shape is 4 rules:
   ;; it's 4
   [[3 0  5]
    [0 2  0]
    [3 0  5]]

   [[5 0  5]
    [0 2 0]
    [3 0  3]]

   [[3 0  3]
    [0 2 0]
    [5 0  5]]

   [[5 0  3]
    [0 2  0]
    [5 0  3]]])


(defn MAS-mask
  [grid]
  (torch/tensor
   (vec (map (fn [row] (vec (map #(get mas-map % 0) row)))
             grid))
   :dtype
   torch/float))

;; ok that is board
(MAS-mask grid)

(play-ca
 (->weights (first MAS-rules))
 (MAS-mask grid))

;; bit messy
(defn count-xmas
  [rule grid]
  (torch/numel (torch/nonzero (py.. (view -1)))))

(defn find-xmasses
  [rule grid]
  (torch/eq (play-ca (->weights rule) (MAS-mask grid))
            (apply + (map #(* % %) (map mas-map "MMASS")))))

(play-ca
 (->weights
  (first MAS-rules))
 (MAS-mask
  [[\M \. \S]
   [\. \A \.]
   [\M \. \S]]))

(find-xmasses
 (->weights
  (first MAS-rules))
 [[\M \. \S]
  [\. \A \.]
  [\M \. \S]])

(defn count-nonzero
  [t]
  (torch/numel (torch/nonzero (py.. t (view -1)))))

(defn part-2
  [input]
  (count-nonzero
    (let [grid (->grid input)]
      (torch/sum (torch/stack
                   (into []
                         (map (fn [rule]
                                (find-xmasses rule grid))
                           MAS-rules)))
                 :dim 0
                 :keepdim true))))

(part-2 ".M.S......\n..A..MSMS.\n.M.S.MAA..\n..A.ASMSM.\n.M.S.M....\n..........\nS.S.S.S.S.\n.A.A.A.A..\nM.M.M.M.M.\n..........\n")

(part-2
 (slurp "/home/benj/repos/advent-of-code/inputs/2024/4/input"))
2041

;; 🎉 🎉🎉🎉🎉
;;
















;; [[0 1 0 2 0 0 0 0 0 0]
;;  [0 0 1 0 1 0 0 4 0 0]
;;  [0 0 1 0 0 0 0 0 0 0]
;;  [0 0 0 1 0 0 0 0 0 0]]

;; .M.S......
;; ..A..MSMS.
;; .M.S.MAA..
;; ..A.ASMSM.
;; .M.S.M....
;; ..........
;; S.S.S.S.S.
;; .A.A.A.A..
;; M.M.M.M.M.
;; ..........



(count (filter #{\A}
               ".M.S......\n..A..MSMS.\n.M.S.MAA..\n..A.ASMSM.\n.M.S.M....\n..........\nS.S.S.S.S.\n.A.A.A.A..\nM.M.M.M.M.\n..........\n"))
;; every single A, but not more.













;; lol, 7
;; now it's too little!
;; hm, maybe now a bug with the rules appears.
;;





;; ah there is one with 2 and one with 4?
;; guess that is the difference of 2 somehow.
;; let's see.





;; .... 11 ?
;; should be 9

;;
;; I'm counting one 3 times, or two 2 times
;; ok so
;; 1. first apply rules,
;;    obtain A masks (where A is part of a X-MAS)
;; 2. then take the bitwise and of all those together
;; 3. count numel again
;;










;; I need to solve this problem:
;;
;;
;; M   S
;;   A
;; M   S
;;
;; this counts
;;
;;
;; M   S
;;   A
;; S   M
;;
;; and this should not count
;;
;; So I need to make sure I don't just count the neighbours
;;




























;; try with the play input:

(let [grid [[\. \. \X \. \. \.]
            [\. 0 \A \M \X  \.]
            [\. \A \. \. \A \.]
            [\X \M \A 0 \.  \S]
            [\. 0 \. \. \. \.]]]
  (apply +
         (map (fn [rule] (count-xmas rule grid target-word))
              rules)))

(def d-rule
  [[1 0 0]
   [0 0 0]
   [0 0 0]])
(def grid
  [[\. \. \X \. \. \.]
   [\. 0 \A \M \X  \.]
   [\. \A \. \. \A \.]
   [\X \M \A 0 \.  \S]
   [\. 0 \. \. \. \.]])

(def mask (mask-for-letter grid \X))

;; [[0. 0.  0. 0. 0.]
;;  [0. 0. 0. 0. 1. 0.]
;;  [0. 0. 0. 0. 0. 0.]
;;  [1. 0. 0. 0. 0. 0.]
;;  [0. 0. 0. 0. 0. 0.]]

;; play rule
(play-ca (->weights d-rule) mask)

[[0. 0. 0. 0. 0. 0.]
 [0. 0. 0. 1. 0. 0.]
 [0. 0. 0. 0. 0. 1.]
 [0. 0. 0. 0. 0. 0.]
 [0. 1. 0. 0. 0. 0.]]
;; ok that looks good...
;; then you bitwise and with the next letter

(def mask2 (mask-for-letter grid \M))

[[0., 0., 0., 0., 0., 0.],
 [0., 0., 0., 1., 0., 0.],
 [0., 0., 0., 0., 0., 0.],
 [0., 1., 0., 0., 0., 0.],
 [0., 0., 0., 0., 0., 0.]]

(play-ca (->weights d-rule) mask)

(let [letter-mask (mask-for-letter grid \M)
      m (play-ca (->weights d-rule)
                 (mask-for-letter grid \X))]
  (torch/bitwise_and
   (py.. m (to :dtype torch/int8))
   (py.. letter-mask
     (to :dtype torch/int8))))

;; hm that also looks correct

;; wait, I am playing 1 too often
;; let me fix this play rule function
(play-rule d-rule grid target-word)

;; ok, I am missing the diagonal


3
;; should be 4
;; aha!









;; so this should be the positions of all M
;; where there was an X before it

;; no, there is some bug

;; trying a simpler example
(play-ca
 (->weights rule)
 (torch/tensor
  [[0 0 0]
   [0 0 0]
   [0 0 0]])
 mask)
;; or look at the last line only















(defn play-xmas-rule
  [xmas-string grid]
  ;; ah
  (reduce
   (fn [state letter]


     )
   {:mask nil}
   xmas-string))

(reduce (fn [state letter] (conj state {:letter letter}))
        ;; {:mask :start}
        [:foo]
        "XMAS")



(defn play-ca
  "Returns a new board with CA weights applied."
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



;; for reference...
(comment
  (let [filters (py.. (torch/tensor [[0 0 0] [1 0 0]
                                     [0 0 0]]
                                    :dtype
                                    torch/float)
                  (view 1 1 3 3))
        inputs (py.. (torch/tensor
                      ;; board:
                      [[0 0 0] [0 1 0] [0 0 0]]
                      :dtype
                      torch/float)
                 (view 1 1 3 3))]
    (py.. (F/conv2d inputs filters :padding 1) (view 3 3)))



  (play-ca
   (torch/tensor
    [[0 0 0]
     [0 0 0]
     [1 0 0]]
    :dtype
    torch/float)

   (torch/tensor
    ;; board:
    [[0 0 0]
     [0 1 0]
     [0 0 0]]
    :dtype
    torch/float))



  ;; yea, that is the one that goes diagonally up.



  ;; ok, something like that

  ;; ----------------------------------

  (->grid example-input)


  ;; ah wait, there is a M after an X
  ;; so the last line should say
  [0 0 1 0 0 0 1 0 0 0]
  [0 0 1 0 0 0 1 0 0 0]
  ;; this is correct.

  ;; ah. I was looking at the page wrong.





  )
