(ns pongserv.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def ^:const WIDTH  800)
(def ^:const HEIGHT 800)
(def ^:const BALL-RADIUS 12) ;; the ball is square ;)

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
  {:p (map #(/ % 3) [WIDTH HEIGHT])
   :v [SPEED_FACTOR (- SPEED_FACTOR)]})

(defn update-state [{:keys [v p]}]
  {:p (map + v p)
   :v (update-v v p BALL-RADIUS WIDTH HEIGHT)})

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 0 0 0)
  ; Set circle color.
  (q/fill 255 255 255)
  (let [[x y]   (:p state)
        [dx dy] (:v state)]
    ;; (println x y dx dy)
    (q/rect (- x BALL-RADIUS) (- y BALL-RADIUS) (* BALL-RADIUS 2) (* BALL-RADIUS 2))))

(q/defsketch pongserv
  :title "Let us play Pong."  :size [WIDTH HEIGHT]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
