
(ns
    day7
    (:require
     [clojure.string :as str]
     [clojure.set :as set]))





(def example
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def
  input
  "bm-XY
ol-JS
bm-im
RD-ol
bm-QI
JS-ja
im-gq
end-im
ja-ol
JS-gq
bm-AF
RD-start
RD-ja
start-ol
cj-bm
start-JS
AF-ol
end-QI
QI-gq
ja-gq
end-AF
im-QI
bm-gq
ja-QI
gq-RD")


(defn
  nodes
  [input]
  (->>
   input
   str/split-lines
   (map #(str/split % #"-"))
   (mapcat
    (fn
      [[a b]]
      [{a #{b}} {b #{a}}]))
   (apply merge-with set/union)))

(defn edges [node nodes]
  (set/difference (nodes node) #{"start"}))

(defn paths [path all-paths nodes]
  (println path)
  (when-not (all-paths path)
    (if
        (>= (->>
             path
             (remove #{"start" "end"})
             (filter (fn [s] (= s (str/lower-case s))))
             count)
            2)
        nil
        (case (last path)
          "end" path
          (into
           []
           (comp
            (map #(paths (into path [%]) nodes all-paths))
            (keep identity))
           (edges (last path) nodes))))))


(defn lower-case-count [path]
  (->>
   path
   (remove #{"start" "end"})
   (filter (fn [s] (= s (str/lower-case s))))
   count))


(defn paths
  [nodes]
  (let [visit (fn visit [[ret path :as acc] node]
                (cond
                  (> (lower-case-count path) 1) acc
                  (= node "end") acc
                  (some #(% path) ret) acc
                  :else
                  (let [[ret path :as acc]
                        (reduce visit [ret (conj path node)] (edges node nodes))]
                    [ret path])))]
    (first (reduce visit [[] []] ["start"]))))

(let [n 1]
  (some #(% n) [#{1}]))




(->>
 example
 ;; input
 nodes
 ;; (edges "start")
 ;; (paths ["start"] #{})
 (paths)
 ;; (patition-by #{"end"})
 ;; (filter #{"end"})
 ;; count
 )
