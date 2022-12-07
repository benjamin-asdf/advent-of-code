(ns Y2022.day7
  (:require [clojure.string :as str]))

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

;; parse a tree

;; (defn ->tree [name childs] {name childs})

(defn ->leaf [name size] {:name name :size size})

(defn parse-dirs [input]
  (:dirs
   (reduce
    (fn [{:keys [curr-path dirs] :as state} line]
      (let [[_  cd-dir] (re-find #"\$ cd ([\w/]+)$"  line)
            [_ size file-name] (re-find #"^(\d+) (.+?)$" line)]
        (cond cd-dir
              (update state :curr-path conj cd-dir)
              (=  "$ cd .." line)
              (update state :curr-path pop)
              file-name
              (assoc
               state
               :dirs
               (update-in
                dirs
                (into curr-path [:files])
                conj
                (->leaf file-name (parse-long size))))
              ;; $ ls
              ;; or
              ;; dir a
              :else
              state)))
    {:dirs {} :curr-path []}
    (str/split-lines input))))


(def tree
  '{"/" {:files ({:name "c.dat", :size 8504156} {:name "b.txt", :size 14848514}),
         "a" {:files ({:name "h.lst", :size 62596}
                      {:name "g", :size 2557}
                      {:name "f", :size 29116}),
              "e" {:files ({:name "i", :size 584})}},
         "d" {:files ({:name "k", :size 7214296}
                      {:name "d.ext", :size 5626152}
                      {:name "d.log", :size 8033020}
                      {:name "j", :size 4060174})}}})


;; do some calculations on the tree

(def at-most 100000)

(defn dir-size [tree]
  ;; I guess I did my tree wrong I want to say something like (tree-seq :dir :files)
  (transduce (keep :size) + 0 (tree-seq seqable? seq tree)))

(dir-size (get-in tree ["/" "a"]))
94853

(->> (parse-dirs (slurp "inputs/2022/day7"))
     (tree-seq seqable? seq)
     (filter map?)
     (remove :name)
     (map dir-size)
     (filter #(<= % at-most))
     (reduce +))

2104783

(defn part-2 [input]
  (let [dirs
        (->> (parse-dirs input)
             (tree-seq seqable? seq)
             (filter map?)
             (remove :name)
             (map dir-size))
        curr-available (- 70000000 (reduce max dirs))
        needed (-  30000000 curr-available)]
    (first (drop-while #(< % needed) (sort dirs)))))


(part-2 (slurp "inputs/2022/day7"))
5883165
