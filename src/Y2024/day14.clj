(ns Y2024.day14
  (:require
   [quil.middleware :as m]
   [quil.core :as q]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [emmy.env :as e]))

;; -------------------------------------------


;; day 14 part 2.
;; christmas tree?

;; 🎄 🎄 🎄


(def robots
  (into
   []
   (map vec
        (partition
         2
         (map vec
              (partition
               2
               (map parse-long
                    (re-seq
                     #"-?\d+"
                     (slurp
                      ;; "/home/benj/repos/advent-of-code/inputs/2024/14/example"
                      "/home/benj/repos/advent-of-code/inputs/2024/14/inputs")))))))))

#_(def width 11)
(def width 101)
#_(def height 7)
(def height 103)

(defn torus-grid [[x y]]
  [(e/modulo x width)
   (e/modulo y height)])

(defn update-position [[p v]] [(e/+ p v) v])

(defn play-rounds
  [robots n]
  (let [play-round (fn [robots]
                     (->> robots
                          (map update-position)
                          (map #(update % 0 torus-grid))))
        rounds n]
    (last (take (inc rounds) (iterate play-round robots)))))

(defn quadrant
  [[x y]]
  (cond (and (< x (e/quotient width 2))
             (< y (e/quotient height 2)))
        :tl
        (and (< (e/quotient width 2) x)
             (< y (e/quotient height 2)))
        :tr
        (and (< x (e/quotient width 2))
             (< (e/quotient height 2) y))
        :bl
        (and (< (e/quotient width 2) x)
             (< (e/quotient height 2) y))
        :br
        :else nil))

(def robot-position first)

;; part-1
(comment
  (apply * (vals (frequencies (keep quadrant
                                    (map robot-position
                                         (play-rounds robots 100)))))))



;; --------------------------------

(defn update-robots
  [{:as state :keys [robots history]}]
  (-> state
      (update :robots play-rounds 1)
      (update :robot-round inc)
      ;; (update :history (fnil conj []) robots)
      ))

(defn update-sketch
  [state]
  ;; (def the-round (:robot-round state))
  #_(cond-> state
      (not (:paused? state))
      (update-robots)
      #_(:paused? state)
      #_(assoc
         :robots
         (play-rounds robots (:robot-round state))))
  #_(update state :robot-color)
  (update
   state
   :robot-size
   (fn [[x y]]
     (e/* (rand-nth
           [0.9 1.1])
          [x y])))
  state)

;; --------------------------------
;; 1 sec the size of the grid
;; christmas tree?

(defn draw-robots
  [{:keys [robots robot-size]}]
  (doseq [[x y] (map robot-position robots)]
    (let [pos [(e/* (first robot-size) x)
               (e/* (second robot-size) y)]]
      (q/with-translation
        pos
        (q/with-fill (apply q/color
                       (rand-nth [[37 190 44] [255 188 0]
                                  [255 255 0]]))
                     (q/rect 0
                             0
                             (first robot-size)
                             (second robot-size)
                             5))))))

(defn draw
  [state]
  (q/background 0)
  ;; (q/color-mode (rand-nth [:rgb :hsb]))
  (draw-robots state))

(defn setup
  []
  (q/frame-rate 5)
  #_(q/rect-mode)
  #_(q/color-mode :hsb)
  (q/color-mode :rgb)
  (q/background 0)
  {:robot-round 0
   :robot-size
   [(/ (q/width) width)
    (/ (q/height) height)]
   :paused? false
   :robots (:robots the-robots)
   ;; robots
   })

(defn forward-time [state]
  (update-robots state))

(defn backward-time
  [state]
  (let [round-n (dec (:robot-round state))]
    (-> state
        (assoc :robot-round round-n))))

(defn pause-time [state] (assoc state :paused? true))
(defn un-pause-time [state] (assoc state :paused? false))

(comment
  (q/sketch :size [1010 1030]
            :middleware [m/fun-mode m/navigation-2d]
            :draw #'draw
            :setup #'setup
            :title "day14-christmas"
            :key-pressed
            (fn [state event]
              (case (:key event)
                :j (-> (backward-time state)
                       (pause-time))
                :k (-> (forward-time state)
                       (pause-time))
                :l (un-pause-time state)
                :h (pause-time state)
                state))
            :update #'update-sketch))

;; -------------------------------

(defn prints-robots [robots]
  (let [robot-position?
        (into #{} (map robot-position) robots)]
    (with-out-str
      (doseq [x (range width)]
        (do
          (doseq [y (range height)]
            (print
             (if (robot-position? [x y])
               "X"
               ".")))
          (println))))))

(defn contigous-robots?
  [{:keys [robots]}]
  (str/includes? (prints-robots robots) "XXXXXXXX"))

(defn part-2
  [robots]
  (let [play-round
          (fn [{:keys [robots n]}]
            {:n (inc n)
             :robots (->> robots
                          (map update-position)
                          (map #(update % 0 torus-grid)))})]
    (first (filter contigous-robots?
                   (iterate play-round {:n 0 :robots robots})))))

(def the-robots (part-2 robots))
