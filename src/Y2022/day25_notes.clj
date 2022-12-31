(ns
    Y2022.day25
    (:require
     [clojure.string :as str]
     [clojure.core.logic.pldb
      :as
      pldb]
     [clojure.core.logic.fd :as fd]
     [clojure.core.logic :as logic]))

(logic/defne counto [lst result]
  (logic/conde
   [(logic/emptyo lst) (logic/== result 0)]
   [(logic/fresh [h t s]
      (logic/conso h t lst)
      (counto t s)
      (logic/== result (+ h s)))]))

(logic/run*
  [q]
  (logic/conde
   [logic/succeed]
   [(logic/== q 11)]
   [logic/succeed
    (logic/== q 10)]))

(logic/run*
  [q]
  (logic/conde
   [logic/succeed]
   [(logic/== q 11)]
   [logic/succeed
    (logic/== q 10)]))

(logic/defne counto
  [result lst]
  ([d []]
   (logic/== d 0))
  ([result [head . tail]]
   (logic/fresh [d]
     (counto d tail)
     (fd/+ d 1 result))))

(logic/run* [q]
  (counto q (range 10)))

(logic/defne sum
  [result lst]
  ([d []]
   (logic/== d 0))
  ([result [head . tail]]
   (logic/fresh [d]
     (sum d tail)
     (fd/+ d head result))))

(logic/run* [q]
  (sum q [1 2 3]))

(logic/defne snafu-value [order e result-so-far result]
  ([order \2 _]
   (logic/fresh [v]
     (fd/* (* 2 5) order v)
     (fd/+ v result-so-far result)))
  ([order \1 _]
   (logic/fresh [v]
     (fd/* (* 1 5) order v)
     (fd/+ v result-so-far result)))
  ([order \0 _]
   (logic/fresh [v]
     (logic/== result result-so-far)))
  ([order \- _]
   (fd/* (* -1 5) order result))
  ([order \- _]
   (logic/fresh [v]
     (fd/* (* 1 5) order v)
     (fd/- result-so-far v result)))
  ([order \= _] (fd/* (* -2 5) order result))
  ([order \0 _] (logic/== result 0)))

(logic/defne snafu-to-decimal
  [result snafu]
  ([d []]
   (logic/== d 0))
  ([result [head . tail]]
   (logic/fresh [d v order]
     (snafu-to-decimal d tail)
     (counto order tail)
     (snafu-value order head result)
     (fd/+ d ))))

(logic/run* [q]
  ;; (snafu-to-decimal q (seq "20"))
  ;; (snafu-to-decimal q (seq "20"))
  (snafu-to-decimal q (seq "2=")))

(logic/run 1 [q] (foo q))

(logic/run 1 [q]
  (logic/fresh [a]
    (logic/trace-lvars "foo" a q)
    (foo a)
    (fd/- a q 200)))


(logic/defne foo [e]
  ([_]
   (logic/fresh [v]
     (fd/== v 10)
     (fd/* v 100 e))))

(logic/run 1 [q]
  (logic/fresh [a]
    (foo a)
    (fd/- a q 200)))

(logic/run 1 [q]
  (logic/== q 10))
