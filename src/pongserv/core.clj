(ns pongserv.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def ^:const WIDTH  800)
(def ^:const HEIGHT 800)

(def ^:const BALL-RADIUS     12) ;; the ball is square ;)
(def ^:const PADDLE-HEIGHT   80)
(def ^:const PADDLE-DEPTH    24)
(def ^:const PADDLE-DISTANCE 50)

(def ^:const SPEED_FACTOR 4)

(defn update-v [v p radius width height]
  (let [[x y]   p
        [dx dy] v
        [x' y'] (map + v p)]
    [(if (or (<= (- x' radius) 0) (>= (+ x' radius) width))
       (- dx)
       dx)
     (if (or (<= (- y' radius) 0) (>= (+ y' radius) height))
       (- dy)
       dy)]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; setup function returns initial state
  {:p (map #(/ % 5) [WIDTH HEIGHT])
   :v [SPEED_FACTOR (- (* 2  SPEED_FACTOR))]
   :left-paddle  (/ HEIGHT 2)
   :right-paddle (/ HEIGHT 2)})

(defn update-state [{:keys [v p left-paddle right-paddle]}]
  {:p (map + v p)
   :v (update-v v p BALL-RADIUS WIDTH HEIGHT)
   :left-paddle  left-paddle
   :right-paddle right-paddle})

(defn centred-rect [x y width height]
  [(- x (/ width 2)) (- y (/ height 2)) width height])

(defn paddle-rect [side centre-y]
  
  (centred-rect
   (if (= :left side)
     PADDLE_DISTANCE
     (- WIDTH PADDLE-DISTANCE))
   centre-y
   PADDLE-DEPTH
   PADDLE-HEIGHT))

(defn draw-state [state]

  (q/background 0 0 0)

  (q/fill 255 255 255)
  
  (let [[x y]   (:p state)]
    ;; (println x y dx dy)
    (q/rect (- x BALL-RADIUS) (- y BALL-RADIUS) (* BALL-RADIUS 2) (* BALL-RADIUS 2))
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
