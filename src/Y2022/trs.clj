(ns
    Y2022.trs
    (:require
     [clojure.string :as str]
     [clojure.core.async :as a]
     [clojure.core.logic.pldb
      :as
      pldb]
     [clojure.core.logic.fd :as fd]
     [clojure.core.logic :as logic]))

(logic/run 2 [x]
  (logic/conde
   [(logic/== x 'extra) logic/s#]
   [(logic/== x 'virgin) logic/u#]
   [(logic/== x 'olive) logic/s#]
   [(logic/== x 'oil) logic/u#]
   [logic/s# logic/u#]))

(logic/run* [r]
  (logic/fresh [x y]
    (logic/conde
     [(logic/== 'split x) (logic/== 'pea y)]
     [(logic/== 'navy x) (logic/== 'bean y)]
     [(logic/== (logic/resto x (logic/resto y '())) r)])))


(logic/run* [r]
  (logic/fresh [x y]
    (logic/conde
     [(logic/== 'split x) (logic/== 'pea y)]
     [(logic/== 'navy x) (logic/== 'bean y)]
     [(logic/== (logic/lcons x (logic/lcons y '())) r)])))

(logic/run* [r]
  (logic/fresh [x y]
    (logic/== 'split x)
    (logic/== 'pea y)
    (logic/== (logic/lcons x (logic/lcons y '())) r)))

(logic/run* [r]
  (logic/fresh [x y]
    (logic/conde
     [(logic/== 'split x) (logic/== 'pea y)]
     [(logic/== 'foo x) (logic/== 'bar y)])
    (logic/== (logic/lcons x (logic/lcons y '())) r)))

(logic/run* [r]
  (logic/fresh [x y]
    (logic/conde
     [(logic/== 'split x) (logic/== 'pea y)]
     [(logic/== 'foo x) (logic/== 'bar y)])
    (logic/== (logic/lcons x (logic/lcons y '(soup))) r)))

(defn teacupo [x]
  (logic/conde
   [(logic/== 'tea x) logic/succeed]
   [(logic/== 'cup x) logic/succeed]))

(logic/run* [r] (teacupo r))
(logic/run 1 [r] (teacupo r))

(logic/run* [r]
  (logic/fresh [x y]
    (logic/conde
     [(teacupo x) (logic/== true y) logic/succeed]
     [(logic/== false x) (logic/== true y)])
    (logic/== (logic/lcons x (logic/lcons y '())) r)))

(logic/run* [r]
  (logic/fresh [x y z]
    (logic/conde
     [(logic/== y x) (logic/fresh [x] (logic/== x z))]
     [(logic/fresh [x]
        (logic/== y x)
        (logic/== z x))])
    (logic/== (logic/lcons x (logic/lcons z '())) r)))

(logic/run* [r]
  (logic/fresh [x y z]
    (logic/conde
     [(logic/== y x) (logic/fresh [x] (logic/== z x))]
     [(logic/fresh [x] (logic/== y x) (logic/== z x))])
    (logic/== false x)
    (logic/== (logic/lcons y (logic/lcons z '())) r)))


(logic/run* [q]
  (let [a (logic/== true q)
        b (logic/== false q)]
    b))

(let [x (fn [a] a)
      y 'c]
  (x y))

(logic/run* [r]
  (logic/fresh [y x]
    (logic/== [x y] r)))

(logic/run* [r]
  (logic/fresh [v w]
    (logic/== (let [x v y w] [x y]) r)))

(logic/run* [r]
  (logic/firsto '[a c o r n] r))


(logic/run* [r]
  (logic/fresh [x y]
    (logic/firsto [r y] x)
    (logic/== x 'pear)))

(logic/run* [r]
  (logic/fresh [x y]
    (logic/firsto '(grape raisin pear) x)
    (logic/firsto '((a) (b) (c)) y)
    (logic/== (logic/lcons x y) r)))

(logic/run* [r]
  (logic/fresh [x y]
    (logic/firsto '(grape raisin pear) x)
    (logic/firsto '((a) (b) (c)) y)
    (logic/== (list x y) r)))

(logic/run* [r]
  (logic/fresh [v]
    (logic/resto '(a c o r n) v)
    (logic/firsto v r)))

(logic/run* [x]
  (logic/conso  x '(a b c) '(d a b c)))

(logic/run* [x]
  (logic/conso  x '(a b c) '(d a b c)))

(logic/run* [x]
  (logic/conso  x '(a b c) '(d a b c)))
