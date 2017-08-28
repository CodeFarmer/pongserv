(ns pongserv.main
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [pongserv.core :refer [centred-rect rects-collide? translate]]))

(def ^:const WIDTH  800)
(def ^:const HEIGHT 800)

(def ^:const BALL-SIZE       24)
(def ^:const PADDLE-HEIGHT   80)
(def ^:const PADDLE-DEPTH    24)
(def ^:const PADDLE-DISTANCE 50)

(def ^:const SPEED_FACTOR 4)

(defn paddle-rect [side centre-y]
  
  (centred-rect
   (if (= :left side)
     PADDLE-DISTANCE
     (- WIDTH PADDLE-DISTANCE))
   centre-y
   PADDLE-DEPTH
   PADDLE-HEIGHT))

(defn ball-rect [x y]
  (centred-rect x y BALL-SIZE BALL-SIZE))

(defn update-v [v ball court-width court-height & obstacle-rects]
  (let [[dx dy] v
        horiz-ball (translate ball dx 0)  [x' _ w' _] horiz-ball
        vert-ball  (translate ball 0  dy) [_ y' _ h'] vert-ball]
    [(if (or (>= (+ x' w') court-width)
             (<= x' 0)
             (some #(rects-collide? horiz-ball %) obstacle-rects))
       (- dx)
       dx)
     (if (or (>= (+ y' h') court-height)
             (<= y' 0)
             (some #(rects-collide? vert-ball %) obstacle-rects))
       (- dy)
       dy)]))

(defn setup []

  (q/frame-rate 30)

  {:p (map #(/ % 5) [WIDTH HEIGHT])
   :v [SPEED_FACTOR (- (* 2  SPEED_FACTOR))]
   :left-paddle  (/ HEIGHT 2)
   :right-paddle (/ HEIGHT 2)})

(defn update-state [{:keys [v p left-paddle right-paddle]}]

  (let [ball (apply ball-rect p)
        lpr  (paddle-rect :left left-paddle)
        rpr  (paddle-rect :right right-paddle)
        v'   (update-v v ball WIDTH HEIGHT lpr rpr)]
    
    {:p (map + v' p)
     :v v'
     :left-paddle  left-paddle
     :right-paddle right-paddle}))

(defn draw-state [state]

  (q/background 0 0 0)

  (q/fill 255 255 255)
  
  (let [[x y]   (:p state)]
    ;; (println x y dx dy)
    (apply q/rect (ball-rect x y))
    (apply q/rect (paddle-rect :left  (:left-paddle state)))
    (apply q/rect (paddle-rect :right (:right-paddle state))))

  (let [y (:left-paddle state)]))

(q/defsketch pongserv
  :title "Let us play Pong."  :size [WIDTH HEIGHT]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
