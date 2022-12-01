(ns
    day7
    (:require
     [clojure.string :as str]
     [clojure.set :as set]))


(def example
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(defn signals [input]
  (->>
   (str/split-lines input)
   (map #(str/split % #" "))
   (map (fn [line] [(take 10 line) (rest (drop 10 line))]))

   ;; (map (fn [v] (remove #{"|"} v)))
   ;; (partition 2)
   ))

(defn nums->count []
  {1 #{2}
   4 #{4}
   7 #{3}
   8 #{7}})

((set/map-invert (nums->count)) #{2})

(defn output-vals [signals]
  (map second signals))

(->>
 (slurp "inputs/day8")
 ;; example
 signals
 output-vals
 (into
  []
  (comp
   cat
   (map count)
   (keep
    (fn [i]
      (some (fn [[_ v]] (v i)) (nums->count))))))
 count)

(defn digit->default-display
  "A map of (0,1,2,..9) ->  ([1,2,3,5,6,7],[3,4],[1,3,4,5,7]...)
  Where the value is that default configuration
  for that digit on
  the hypethical (virtual) working display.
  Use 1..7 instead of puzzle input a,b..g"
  []
  {0 #{1 2 3 5 6 7}
   1 #{3 4}
   2 #{1 3 4 5 7}
   3 #{1 3 4 6 7}
   4 #{2 3 4 5}
   5 #{1 2 4 6 7}
   6 #{1 2 4 5 6 7}
   7 #{1 2 4}
   8 #{1 2 3 4 5 6 7}
   9 #{1 2 3 4 6 7}})


;; map char -> nums

;; filter by superset of alreaady known

;; count -> num
