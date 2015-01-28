(ns dotlife.core
  (:require [quil.core :as q]))


(defn distance [a b]
  (let [dx (- (:x a) (:x b))
        dy (- (:y a) (:y b))]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))


(def species
  {:plain     {:size 3 :max-speed 2.7 :vision  60 :change 0.050 :rgb [250 250  10]}
   :predator  {:size 4 :max-speed 3.0 :vision 200 :change 0.050 :rgb [250   0   0]}
   :prey      {:size 3 :max-speed 3.5 :vision  60 :change 0.010 :rgb [ 10 200  10]}
   :cursor    {:size 9 :max-speed 0.0 :vision   0 :change 0.010 :rgb [255 255 255]}})


(defn attraction [myself other]
  (case [(:species myself) (:species other)]
    [:plain    :plain    ]  (- (distance myself other) 20)
    [:plain    :predator ]  -500
    [:plain    :prey     ]  -1
    [:plain    :cursor   ]  0

    [:predator :plain    ]  10
    [:predator :predator ]  -20
    [:predator :prey     ]  10
    [:predator :cursor   ]  -999

    [:prey     :plain    ]  1
    [:prey     :predator ]  -500
    [:prey     :prey     ]  (- (distance myself other) 30)
    [:prey     :cursor   ]  (* 0.5 (- (distance myself other) 30))

    [:cursor   :plain    ]  0
    [:cursor   :predator ]  0
    [:cursor   :prey     ]  0
    [:cursor   :cursor   ]  0))



(comment Even comments can be s-expressions
  (count (filter (fn [dot] (= :prey (:species dot))) @game-state))

  (defn remove-cursor [dots]
    (remove (fn [dot] (= :cursor (:species dot))) dots))

  (defn on-mouse-moved []
    (let [cursor (new-dot :cursor (q/mouse-x) (q/mouse-y))
          add-cursor (fn [dots dot] (conj (remove-cursor dots) dot))]
      (swap! game-state add-cursor cursor)))

  (defn on-mouse-exited []
    (swap! game-state remove-cursor))
)


(defn rand-species []
  (let [r (rand-int 100)]
    (cond
     (< r   4) :predator
     (< r  52) :prey
     (< r 100) :plain)))

(defn new-dot [s x y]
  {:species s
   :x x
   :y y
   :vx (* 5 (- (rand) 0.5))
   :vy (* 5 (- (rand) 0.5))})

(defn rand-dot []
  (new-dot (rand-species) (rand 500) (rand 500)))

(def game-state (atom nil))

(defn clamp [minimum i maximum]
  (cond
   (< i minimum) minimum
   (> i maximum) maximum
   :else i))

(defn wrap [minimum i maximum]
  (cond
   (< i minimum) (+ minimum (- maximum i))
   (> i maximum) (- i maximum)
   :else i))

(defn wrap-dot [dot w h]
  (assoc dot
    :x (wrap 0 (:x dot) w)
    :y (wrap 0 (:y dot) h)))

(defn wrap-dots [dots w h]
  (map #(wrap-dot % w h) dots))

(defn clamp-speed [dot]
  (let [speed (Math/sqrt (+ (* (:vx dot) (:vx dot)) (* (:vy dot) (:vy dot))))
        max-speed (get-in species [(:species dot) :max-speed])]
    (assoc dot
      :vx (if (> speed max-speed) (* (/ (:vx dot) speed) max-speed) (:vx dot))
      :vy (if (> speed max-speed) (* (/ (:vy dot) speed) max-speed) (:vy dot)))))

(defn brownian-motion [dot]
  (let [half-force 0.1
        full-force (* half-force 2)]
    (assoc dot
      :vx (+ (:vx dot) (* full-force (rand)) (- half-force))
      :vy (+ (:vy dot) (* full-force (rand)) (- half-force)))))

(defn move [dot]
  (assoc dot
    :x (+ (:x dot) (:vx dot))
    :y (+ (:y dot) (:vy dot))))

(defn react [dot other]
  (let [dist (distance dot other)
        max-distance (get-in species [(:species dot) :vision])]
    (cond
     (> dist max-distance)
     nil
     (< dist 1)
     nil
     :else
     (let [dx (- (:x dot) (:x other))
           dy (- (:y dot) (:y other))
           multiplier (- 1 (/ dist max-distance))
           valence (* -1 multiplier (attraction dot other))
           valence (clamp -999 valence 999)]
       (if (< -0.01 valence 0.01)
         nil
         {:vx (* valence (/ dx dist))
          :vy (* valence (/ dy dist))})))))

(defn react-to [dot others]
  (let [reactions (keep (partial react dot) others)
        reaction (apply merge-with + reactions)
        change (get-in species [(:species dot) :change])
        inverse-change (- 1 change)]
    (if reaction
      (assoc dot
        :vx (+ (* inverse-change (:vx dot))
               (* change (clamp -20 (:vx reaction) 20)))
        :vy (+ (* inverse-change (:vy dot))
               (* change (clamp -20 (:vy reaction) 20))))
      dot)))

(defn update-dot [dot all]
  (-> dot
      (react-to all)
      (brownian-motion)
      (clamp-speed)
      (move)))

(defn update-state [dots]
  (map (fn [dot] (update-dot dot dots)) dots))

(defn setup []
  (q/smooth)
  (q/frame-rate 30)
  (q/background 20 20 60))

(defn draw []
  (swap! game-state update-state)
  (swap! game-state wrap-dots (q/width) (q/height))
  (q/background 20 20 60)
  (q/no-stroke)
  (doseq [dot (deref game-state)]
    (let [species-data (get species (:species dot))]
      (apply q/fill (:rgb species-data))
      (q/ellipse (:x dot) (:y dot) (:size species-data) (:size species-data)))))

(defn on-mouse-exited []
  nil)

(defn on-mouse-moved []
  nil)

(defn -main []
  (reset! game-state (repeatedly 125 rand-dot))
  (q/defsketch example
    :title "Dot life"
    :setup setup
    :draw draw
    :features [:resizable]
    :mouse-moved on-mouse-moved
    :mouse-exited on-mouse-exited
    :size [500 500]))
