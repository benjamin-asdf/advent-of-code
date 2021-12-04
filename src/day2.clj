(ns day2
  (:require [clojure.string :as str]))


;; part 1

(def input
  (str/split-lines
   (slurp "day2-directions")))

(def
  example-input
  (str/split-lines
   "forward 5
down 5
forward 8
up 3
down 8
forward 2"))

(defn movements [input]
  (->>
   input
   (map #(str/split % #" "))
   (into
    []
    (mapcat
     (fn [[d n]]
       (repeat (parse-long n) (keyword d)))))))

(let [moves (movements input)
      position
      {:depth
       (-
        (count (filter #{:down} moves))
        (count (filter #{:up} moves)))
       :horizontal (count (filter #{:forward} moves))}]
  (* (:depth position) (:horizontal position)))
;; 1990000


;; part 2

;; aim is what we thought of as depth earlier

(defn
  aim
  [moves]
  (-
   (count (filter #{:down} moves))
   (count (filter #{:up} moves))))

(defn depth [moves]
  (loop [[mv & more :as moves*] (reverse moves)
         depth 0]
    (if-not mv
      depth
      (recur
       more
       (case mv
         :forward
         (+ (aim moves*) depth)
         depth)))))

(let [moves (movements input)]
  (* (depth moves)
     (count (filter #{:forward} moves))))
;; 1975421260


(comment
  (let [moves (movements example-input)]
    (* (depth moves)
       (count (filter #{:forward} moves)))))
