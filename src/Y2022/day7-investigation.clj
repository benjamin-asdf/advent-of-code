;; this wont work if you have dirs with the same name
;; at different paths
(defn dirs->sizes [tree]
  (into
   {}
   (comp
    (filter dir?)
    (map (juxt first dir-size)))
   (tree-seq seqable? seq tree)))

{"/" 48381165, "a" 94853, "e" 584, "d" 24933642}

(defn part-1 [input]
  (->>
   (parse-dirs input)
   dirs->sizes
   (map second)
   (filter #(<= % at-most))
   (reduce +)))

(part-1 input)

95437
(part-1 (slurp "inputs/2022/day7"))

1398510

;; too low
289

(count (filter #(re-find #"^\d" %) (str/split-lines (slurp "inputs/2022/day7"))))

(count (filter #(re-find #"^dir" %) (str/split-lines (slurp "inputs/2022/day7"))))
179
(count (dirs->sizes (parse-dirs (slurp "inputs/2022/day7"))))
118
;; that is correct "dir" + "/"
;; amount of dirs is okk


;; check the overall sum


(dir-size (get (parse-dirs (slurp "inputs/2022/day7")) "/"))

44965705
(reduce + (map parse-long (keep (comp second #(re-find #"^(\d+)" %)) (str/split-lines (slurp "inputs/2022/day7")))))

44965705
;; that also checks out

;; at-most is also correct
;; thought <= vs < error but it is fine

(tree-seq
 seqable?
 seq
 (parse-dirs (slurp "inputs/2022/day7")))


;; ah maybe there are dirs with files

()

(->>

 (tree-seq
  seqable?
  seq
  (parse-dirs (slurp "inputs/2022/day7")))
 (filter  ))

(dirs->sizes
 '{"/"
  {:files ({:name "c.dat", :size 8504156} {:name "b.txt", :size 14848514}),
   "a" {:files ({:name "h.lst", :size 62596}
                {:name "g", :size 2557}
                {:name "f", :size 29116}),
        "e" {:files ({:name "i", :size 584})}},
   "d" {:files ({:name "k", :size 7214296}
                {:name "d.ext", :size 5626152}
                {:name "d.log", :size 8033020}
                {:name "j", :size 4060174})}}})




;; eeehh
(def dir? (fn [e] (and (vector? e) (:files (val e)))))

(defn dirs->sizes [tree]
  (into
   []
   (comp
    (filter dir?)
    (map dir-size))
   (tree-seq seqable? seq tree)))

(defn part-1 [input]
  (->>
   (parse-dirs input)
   dirs->sizes)
  (filter #(<= % at-most))
  (reduce +))

(part-1 input)
95437

(part-1 (slurp "inputs/2022/day7"))
1568760

;; too low, fuck

(->>
   (parse-dirs (slurp "inputs/2022/day7"))
   dirs->sizes)
(filter #(<= % at-most))
(reduce +)


(->>
 (parse-dirs (slurp "inputs/2022/day7"))
 dirs->sizes
 count)
148

(count (filter #(re-find #"^dir" %) (str/split-lines (slurp "inputs/2022/day7"))))
179


;; ah there must be dirs that only have child dirs but no files themselves, fuck
;; so because I didn't to the tree right the `dir?` function was confused and my domain model did not encompas the domain
