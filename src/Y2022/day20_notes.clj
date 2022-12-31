(ns Y2022.day20
  (:require [clojure.string :as str]))


(def input "1
2
-3
3
-2
0
4")

(def encrypted-file (read-string (str "[" input "]")))

(let [encrypted (->>
                 encrypted-file
                 (into []
                       (map (juxt identity (constantly nil)))

                       ))]
  (reduce (fn [moved [n moved?]]
            (if moved? moved
                (into
                 (subvec moved 1 n)
                 (concat [[n true]] (subvec moved n)))))
          encrypted
          encrypted))


split-at

(let [n 0
      moved [1 2 3]]
  [(subvec moved (inc n) ())
   [n]
   (subvec moved (inc (inc n)))])



(let [moved] [1 2 3]

     )



(let [moved
      (->>
       encrypted-file
       (into []
             (map (juxt identity (constantly nil)))

             ))
      n 0]
  (into
   (subvec moved n 1)
   [[n true]]
   (subvec moved n)))



(def mixed-file (mix encrypted-file))

(defn grove-coordinates [mixed-file]
  (let [n (count mixed-file)]
    (list (nth mixed-file (mod 1000 n))
          (nth mixed-file (mod 2000 n))
          (nth mixed-file (mod 3000 n)))))

(def coordinates (grove-coordinates mixed-file))

(apply + coordinates)



(defn mix [encrypted-file]
  (let [n (count encrypted-file)]
    (loop [moved-file (vec encrypted-file)
           pos 0]
      (if (= pos n)
        moved-file
        (let [number (first )
              ]
          (recur
           (assoc moved-file
                  (mod (+ pos (nth moved-file 0)) n)
                  (nth moved-file 0))
           (inc pos)))

        )
      (if (empty? moved-file)
        moved-file
        ))))
