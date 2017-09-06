(ns pongserv.main
  (:require [clojure.data.json :as json] 
            [quil.core :as q]
            [quil.middleware :as m]
            [pongserv.core :refer [centred-rect rects-collide? until translate]]
            [pongserv.server :refer [create-network-server]])
  (:import  [java.io BufferedReader PrintWriter InputStreamReader OutputStreamWriter]))

(def ^:const WIDTH  800)
(def ^:const HEIGHT 800)

(def ^:const BALL_SIZE       24)
(def ^:const PADDLE_HEIGHT   80)
(def ^:const PADDLE_DEPTH    24)
(def ^:const PADDLE_DISTANCE 50)

(def ^:const PADDLE_SPEED 1)
 ;; how close can the centre of the paddle get to the edge?
(def ^:const PADDLE_LIMIT 60)

(def ^:const SPEED_FACTOR 4)
(def ^:const FPS 30)

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


(defn move-paddle [side paddle]
  (let [input (get (:inputs @game-state) side)]
    (cond
      (= :up input)   (max PADDLE_LIMIT            (- paddle PADDLE_SPEED))
      (= :down input) (min (- HEIGHT PADDLE_LIMIT) (+ paddle PADDLE_SPEED))
      :else paddle)))


(defn draw-state [state]

  (q/background 0 0 0)

  (q/fill 255 255 255)
  
  (let [[x y]   (:p state)]
    (apply q/rect (ball-rect x y))
    (apply q/rect (paddle-rect :left  (:left-paddle state)))
    (apply q/rect (paddle-rect :right (:right-paddle state))))

  (let [y (:left-paddle state)]))


(defn new-player [side socket]

  {:side side
   :input  (-> socket
               .getInputStream
               InputStreamReader.
               BufferedReader.) 
   :output (-> socket
               .getOutputStream
               OutputStreamWriter.
               PrintWriter.)
   :socket socket})


(defn send-message [player message]
  (let [out (:output player)]
    (.println out message)
    (.flush out)))

(defn read-message [player]
  (-> player
      :input
      .readLine
      .trim
      .toLowerCase))


(defn client-handler [state-atom socket]

  (defn join-game [player]
    (swap! state-atom assoc-in [:players (:side player)] player))

  (defn leave-game [player]
    (swap! state-atom assoc-in [:players (:side player)] nil))

  (defn set-input-state [player direction]
    (swap! state-atom assoc-in [:inputs (:side player)] direction))

  (let [{:keys [left right]} (:players @state-atom)
        player (cond
                 (not left)  (new-player :left  socket)
                 (not right) (new-player :right socket)
                 :else nil)]
    (if player
      
      (do
        (join-game player)
        (try
          (loop [p player]
            
            (let [message (read-message player)]
              
              (cond
                (= "u" message) (set-input-state player :up)
                (= "d" message) (set-input-state player :down)
                (= "x" message) (set-input-state player :stop)))
            
            (Thread/sleep 10) ;; avoid flooding the server
            (recur p))
          
          (catch Exception e
            (leave-game player))))

      (let [os (PrintWriter. (OutputStreamWriter. (.getOutputStream socket)))]
        (.println os "Game is full, come back later")
        (.flush os)
        (.close socket)))))

(defn update-state [{:keys [v p left-paddle right-paddle]}]

  (let [ball (apply ball-rect p)
        lpr  (paddle-rect :left left-paddle)
        rpr  (paddle-rect :right right-paddle)
        v'   (update-v v ball WIDTH HEIGHT lpr rpr)
        new-state 
        
        {:p (map + v' p)
         :v v'
         :left-paddle  (move-paddle :left  left-paddle)
         :right-paddle (move-paddle :right right-paddle)}]

    (doseq [side (keys (:players @game-state))]
        (if-let [player (get (:players @game-state) side)]
          (send-message player
                        (json/write-str 
                         {:height HEIGHT
                          :width WIDTH
                          :side side
                          :ball (:p new-state)
                          :left (:left-paddle new-state)
                          :right (:right-paddle new-state)}))))

    new-state))


(defn setup! []

  (q/frame-rate FPS)

  (swap! game-state assoc :server-socket (create-network-server game-state client-handler))

  {:p (map #(/ % 5) [WIDTH HEIGHT])
   :v [SPEED_FACTOR (- (* 2  SPEED_FACTOR))]
   :left-paddle  (/ HEIGHT 2)
   :right-paddle (/ HEIGHT 2)})

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

(q/defsketch pongserv
  :title "Let us play Pong."
  :size [WIDTH HEIGHT]
  :setup setup!
  :update update-state
  :draw draw-state
  :features [(comment :keep-on-top)]
  :middleware [m/fun-mode]
  :on-close shutdown!)
