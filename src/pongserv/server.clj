(ns pongserv.server
  (:require [pongserv.core :refer [in-thread until]])
  (:import [java.io BufferedReader InputStreamReader OutputStream]
           [java.net InetAddress ServerSocket Socket SocketException]))


(def ^:const PORT 6000)

(defn create-network-server
  [game-state-atom client-handler]
  (let [server-socket (ServerSocket. PORT)]
    (in-thread
     (try
       (until (.isClosed server-socket)
         (let [socket (.accept server-socket)]
           (in-thread
            (client-handler game-state-atom socket))))
       (catch SocketException e (println e))))
    server-socket))
