(ns pongserv.player
  (:import  [java.io BufferedReader PrintWriter InputStreamReader OutputStreamWriter]))


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

(defn new-player [side socket]

  (let [p {:side side
           :input  (-> socket
                       .getInputStream
                       InputStreamReader.
                       BufferedReader.) 
           :output (-> socket
                       .getOutputStream
                       OutputStreamWriter.
                       PrintWriter.)
           :socket socket
           :score 0}]

    (send-message p "Who are you?")
    (assoc p :name (read-message p))))


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
