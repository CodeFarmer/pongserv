(ns pongserv.main
  (:require [clojure.data.json :as json] 
            [quil.core :as q]
            [quil.middleware :as m]
            [pongserv.player :refer [client-handler new-player read-message send-message]]
            [pongserv.core :refer [centred-rect rects-collide? until translate]]
            [pongserv.server :refer [create-network-server]])
  (:gen-class))

(def ^:const WIDTH  800)
(def ^:const HEIGHT 600)

(def ^:const BALL_SIZE       24)
(def ^:const PADDLE_HEIGHT   80)
(def ^:const PADDLE_DEPTH    24)
(def ^:const PADDLE_DISTANCE 50)

(def ^:const SCOREBOARD_HEIGHT 80)

(def ^:const PADDLE_SPEED 2)
 ;; how close can the centre of the paddle get to the edge?
(def ^:const PADDLE_LIMIT 60)

(def ^:const STARTING_V [4 8])
(def ^:const FPS 30)

(def ^:const SPEEDUP_PERIOD_SECONDS 8)
(def ^:const ACCEL_FACTOR 1.1)

(def ^:const WINNING_SCORE 5)

(def game-state (atom {:inputs {:left :stop :right :stop}
                       :players {:left nil :right nil}}))

(defn paddle-rect [side centre-y]
  
  (centred-rect
   (if (= :left side)
     PADDLE_DISTANCE
     (- WIDTH PADDLE_DISTANCE))
   centre-y
   PADDLE_DEPTH
   PADDLE_HEIGHT))

(defn ball-rect [x y]
  (centred-rect x y BALL_SIZE BALL_SIZE))

(defn hitting-left? [v ball]
  (let [[dx _] v
        horiz-ball (translate ball dx 0)  [x' _ _ _] horiz-ball]
    (<= x' 0)))

(defn hitting-right? [v ball court-width]
  (let [[dx _] v
        horiz-ball (translate ball dx 0)  [x' _ w' _] horiz-ball]
    (>= (+ x' w') court-width)))

(defn update-v [v ball court-width court-height & obstacle-rects]

  (let [[dx dy] v
        horiz-ball (translate ball dx 0)  [x' _ w' _] horiz-ball
        vert-ball  (translate ball 0  dy) [_ y' _ h'] vert-ball]

    [(if (or (hitting-left? v ball)
             (hitting-right? v ball court-width)
             (some #(rects-collide? horiz-ball %) obstacle-rects))
       (- dx)
       dx)

     (if (or (>= (+ y' h') court-height)
             (<= y' 0)
             (some #(rects-collide? vert-ball %) obstacle-rects))
       (- dy)
       dy)]))


(defn move-paddle [side paddle]
  (let [input (get (:inputs @game-state) side)]
    (cond
      (= :up input)   (max PADDLE_LIMIT            (- paddle PADDLE_SPEED))
      (= :down input) (min (- HEIGHT PADDLE_LIMIT) (+ paddle PADDLE_SPEED))
      :else paddle)))

(defn draw-scoreboard []

  (let [h (/ SCOREBOARD_HEIGHT 2)]
    (q/text-size h)
    (q/text-align :center)

    (let [{:keys [left right]} (:players @game-state)]

      (if left
        (do
          (q/text (str (:score left)) (/ WIDTH 3) h)
          (q/text (str (:name left)) (/ WIDTH 3) (* 2 h))))

      (if right
        (do
          (q/text (str (:score right)) (* 2 (/ WIDTH 3)) h)
          (q/text (str (:name right)) (* 2 (/ WIDTH 3)) (* 2 h)))))))

(defn draw-state [state]

  (q/background 0 0 0)

  (q/fill 255 255 255)
  
  (draw-scoreboard)

  (q/with-translation [0 SCOREBOARD_HEIGHT]
    (let [[x y]   (:p state)]

      (apply q/rect (ball-rect x y))
      (apply q/rect (paddle-rect :left  (:left-paddle state)))
      (apply q/rect (paddle-rect :right (:right-paddle state)))))

  (if (= :reset (:mode state))
    (q/delay-frame 1000)))

(defn initial-state []

  {:p (map #(/ % 5) [WIDTH HEIGHT])
   :v (map #(* (rand-nth [1 -1]) %) STARTING_V)
   :left-paddle  (/ HEIGHT 2)
   :right-paddle (/ HEIGHT 2)
   :t 0
   :last-speedup 0
   ; mode is :attract or :simple - simple means if the ball goes off the edge, the other guy gets a point
   :mode :attract})

(defn update-state [{:keys [v p left-paddle right-paddle last-speedup t mode]}]

  ;; FIXME this huge let logic is disgusting
  (let [ball (apply ball-rect p)
        lpr  (paddle-rect :left left-paddle)
        rpr  (paddle-rect :right right-paddle)

        v' (update-v v ball WIDTH HEIGHT lpr rpr)
        speedup? (>= t (+ last-speedup (* FPS SPEEDUP_PERIOD_SECONDS)))
        v'' (if speedup? 
              (map #(* ACCEL_FACTOR %) v')
              v')

        left-player (:left (:players @game-state))
        right-player (:right (:players @game-state))

        mode' (cond
                
                (and (= mode :attract) left-player right-player)
                (do (swap! game-state assoc-in [:players :left :score] 0)
                    (swap! game-state assoc-in [:players :right :score] 0)
                    :reset)
                
                (and (= mode :playing) 
                     (not (and left-player right-player))) :attract

                (= mode :playing)
                (do 
                  (cond
                    (and right-player
                         (hitting-left? v ball))
                    (do (swap! game-state update-in [:players :right :score] inc)
                        (if (>= (:score (:right (:players @game-state))) WINNING_SCORE)
                          (.close (:socket left-player)))
                        :reset)
                    (and left-player
                         (hitting-right? v ball WIDTH))
                    (do (swap! game-state update-in [:players :left :score] inc)
                        (if (>= (:score (:left (:players @game-state))) WINNING_SCORE)
                          (.close (:socket right-player)))
                        :reset)

                    :default :playing))
                
                (= mode :reset)
                :playing

                :default mode)

        new-state (cond 
                    (= mode :reset) (assoc (initial-state) :mode :playing) 
                    :default {:p (map + v'' p)
                              :v v''
                              :left-paddle  (move-paddle :left  left-paddle)
                              :right-paddle (move-paddle :right right-paddle)
                              :t (inc t)
                              :last-speedup (if speedup? t last-speedup)
                              :mode mode'})]

    (println mode mode')

    (doseq [side (keys (:players @game-state))]
        (if-let [player (get (:players @game-state) side)]
          (send-message player
                        (json/write-str 
                         {:height HEIGHT
                          :width WIDTH
                          :side side
                          :ball (:p new-state)
                          :left (:left-paddle new-state)
                          :right (:right-paddle new-state)
                          :t t}))))
    new-state))

(defn setup! []

  (q/frame-rate FPS)

  (swap! game-state assoc :server-socket (create-network-server game-state client-handler))

  (initial-state))

(defn shutdown! [_]
  
  (if-let [sock (get @game-state :server-socket)]
    (if (not (.isClosed sock))
      (.close sock)))

  (doseq [side (keys (:players @game-state))]
    (-> @game-state
        :players
        side
        :socket
        .close)))

(defn -main
  [& args]
  (q/sketch :title "Let us play Pong."
            :size [WIDTH (+ HEIGHT SCOREBOARD_HEIGHT)]
            :setup setup!
            :update update-state
            :draw draw-state
            :features [(comment :keep-on-top)]
            :middleware [m/fun-mode]
            :on-close shutdown!))
