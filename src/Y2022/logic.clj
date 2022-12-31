(ns
    Y2022.logic
  (:use [clojure.core.logic :as logic])
  (:require
   [clojure.core.logic.pldb :as pldb]
   [clojure.core.logic.fd :as fd]))

(run* [q]
  (== q true))

(run* [r]
  (fresh [x y]
    (== (lcons x (lcons y 'salad)) r)))


(run* [a b c]
  (+)
  (== q true))

(run*
  [q]
  (membero q [1 2 3])
  (membero q [2 3 4]))

 (logic/run* [q a b c d e]
   (logic/== q (range 10))
   (logic/matche [q]
                 [[[. a b c d e]]]))

(logic/run*
  [q a b c d e]
  (logic/== q (range 10))
  (logic/matche
   [q]
   [[[a . e]]])
  ;; (logic/== a 1)
  )

(logic/run*
  [root]

  (logic/== q (range 10))
  (logic/matche
   [q]
   [[[a . e]]])
  ;; (logic/== a 1)
  )

(def a 10)

 ;;  Showcase, lcons & lvar
(logic/run* [q]
  (logic/matche [q]
                ([[a . b]])))


 ; => ((_0 . _1)) ; a and b are both new, implicit lvar since they are not bound locally and there is no value to unify them with.

 ;;  Showcase, local binding
(let [a 8]
  (logic/run* [q]
    (logic/matche [q]
                  ([[a . b]]))))
 ; => ((8 . _0)) ; a is bound locally so the value is used instead of creating a new lvar.


 ;;  Showcase, defne to create new goals with patterns
(logic/defne head-is-1 [q]
  ([(1 . _)]))

(logic/defne tail-is-7-8-9 [q]
  ([[_ . [7 8 9]]]))

(logic/run 2 [q]
  (tail-is-7-8-9 q)
  (head-is-1 q))

(logic/run* [q]
  (logic/matche [q] ([a])))

(run* [q] (== {:a q :b 2} {:a 1 :b 2}))

(run* [q]
  (fresh [a]
    (membero a [1 2 3])
    (membero q [3 4 5])
    (== a q)))

(run* [q]
  (conde
   [succeed]))

(run* [q]
  (conde
   [succeed succeed succeed succeed]))

(run* [q]
   (conde
     [succeed succeed fail succeed]))

(run* [q]
  (conde
   [succeed]
   [succeed]))

(run* [q]
  (conde
   [succeed]
   [fail]))

(run* [q]
  (conde
   [succeed (== q 1)]))

(run* [q]
  (== q 1))

(run* [q]
  (conde
   [(== q 2) (== q 1)]))

(run* [q]
  (conde
   [(== q 1)]
   [(== q 2)]))

(run* [q]
  (conso 1 [2 3] q))

(run* [q]
  (conso 1 q [1 2 3]))

(run* [q]
  (conso q [2 3] [1 2 3]))

(run* [q]
  (conso 1 [2 q] [1 2 3]))

(run* [q]
  (resto [1 2 3 4] q))

(run* [q]
  (membero 7 [1 3 8 q]))

(run*
  [root a b]
  (== a 1)
  (== b 2)
  (== root (+ a b)))

(run*
  [root a b]
  (== a 1)
  (== b 2)
  (== root ()))

(run* [q] (== q (+ 1 2)))

(run* [q]
  (== a 12)
  (== q (+ 1 a)))

(run* [q]
  (fresh [a b]
    (== a 2)
    (conde [(+ a b)])
    (== q (+ b a)))
  ;; (== 10 (+ 1 a))
  )


(run* [q]
  (fresh [a b]
    (logic/pred q (fn []))
    (membero a [1 2 3])
    (membero b [4 5 6])
    (> q 2)))


(pldb/db-rel man p)
(pldb/db-rel woman p)
(pldb/db-rel likes p1 p2)
(pldb/db-rel fun p)

(def facts0
  (pldb/db
   [man 'Bob]
   [man 'John]
   [man 'Ricky]

   [woman 'Mary]
   [woman 'Martha]
   [woman 'Lucy]

   [likes 'Bob 'Mary]
   [likes 'John 'Martha]
   [likes 'Ricky 'Lucy]))

(def facts1 (-> facts0 (pldb/db-fact fun 'Lucy)))

(pldb/with-db facts1
  (run* [q]
    (fresh [x y]
      (fun y)
      (likes x y)
      (== q [x y]))))


(defne moveo [before action after]
  ([[:middle :onbox :middle :hasnot]
    :grasp
    [:middle :onbox :middle :has]])
  ([[pos :onfloor pos has]
    :climb
    [pos :onbox pos has]])
  ([[pos1 :onfloor pos1 has]
    :push
    [pos2 :onfloor pos2 has]])
  ([[pos1 :onfloor box has]
    :walk
    [pos2 :onfloor box has]]))

(defne cangeto [state out]
  ([[_ _ _ :has] true])
  ([_ _] (fresh [action next]
           (moveo state action next)
           (cangeto next out))))

(run 1 [q]
  (cangeto [:atdoor :onfloor :atwindow :hasnot] q))



(defn get-square [rows x y]
  (for [x (range x (+ x 3))
        y (range y (+ y 3))]
    (get-in rows [x y])))

(defn bind [var hint]
  (if-not (zero? hint)
    (== var hint)
    succeed))

(defn bind-all [vars hints]
  (and* (map bind vars hints)))

(defn sudokufd [hints]
  (let [vars (repeatedly 81 lvar)
        rows (->> vars (partition 9) (map vec) (into []))
        cols (apply map vector rows)
        sqs  (for [x (range 0 9 3)
                   y (range 0 9 3)]
               (get-square rows x y))]
    (run 1 [q]
      (== q vars)
      (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) vars)
      (bind-all vars hints)
      (everyg fd/distinct rows)
      (everyg fd/distinct cols)
      (everyg fd/distinct sqs))))

(def hints
  [9 0 6 0 0 0 0 3 0
   0 0 1 6 0 2 7 0 0
   0 0 2 4 1 0 8 0 0
   0 0 0 1 0 0 0 2 0
   6 0 0 0 8 0 0 0 4
   0 2 0 0 0 6 0 0 0
   0 0 3 0 5 9 2 0 0
   0 0 9 7 0 1 4 0 0
   0 6 0 0 0 0 9 0 7])

(into
 []
 (mapv
  #(into [] %)
  (partition
   9
   (first (sudokufd hints)))))

[[9 4 6 5 7 8 1 3 2]
 [8 3 1 6 9 2 7 4 5]
 [7 5 2 4 1 3 8 6 9]
 [5 9 4 1 3 7 6 2 8]
 [6 1 7 2 8 5 3 9 4]
 [3 2 8 9 4 6 5 7 1]
 [4 7 3 8 5 9 2 1 6]
 [2 8 9 7 6 1 4 5 3]
 [1 6 5 3 2 4 9 8 7]]
