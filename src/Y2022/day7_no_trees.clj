(ns
    Y2022.day7-no-trees
    (:require
     [clojure.string :as str]))
(def
  input
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn dir-sizes [input]
  (->>
      ;; this is the part I was thinking about lying in bed yesterday
   (read-string
    (let [s (->
             input
             (str/replace #"\$ cd (/|\w+)\n"  "( ")
             (str/replace #"\$ cd \.\.\n" ") ")
             (str/replace #"[^\d+ \(\)]" ""))]
      ;; this I did not anticipate haha
      (str
       s
       (apply str
              (repeat
               (-
                (count (re-seq #"\(" s))
                (count (re-seq #"\)" s))) ")")))))
   (tree-seq seqable? seq)
   (filter list?)
   (map
    (fn
      [e]
      (reduce
       +
       (filter
        number?
        (tree-seq seqable? seq e)))))))

;; part 1
(->>
 (slurp "inputs/2022/day7")
 dir-sizes
 (filter #(< % 100000))
 (reduce +))

2104783

;; part 2
(let [dirs
      (->> (slurp "inputs/2022/day7") dir-sizes)
      curr-available (- 70000000 (reduce max dirs))
      needed (-  30000000 curr-available)]
    (first (drop-while #(< % needed) (sort dirs))))

5883165
