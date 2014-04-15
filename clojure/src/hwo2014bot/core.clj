(ns hwo2014bot.core
  (:require [clojure.data.json :as json])
  (:use [aleph.tcp :only [tcp-client]]
        [lamina.core :only [enqueue wait-for-result wait-for-message]]
        [gloss.core :only [string]])
  (:gen-class))

(defn- json->clj [string]
  (json/read-str string :key-fn keyword))

(defn send-message [channel message]
  (enqueue channel (json/write-str message)))

(defn read-message [channel]
  (json->clj
    (try
      (wait-for-message channel)
      (catch Exception e
        (println (str "ERROR: " (.getMessage e)))
        (System/exit 1)))))

(defn connect-client-channel [host port]
  (wait-for-result
   (tcp-client {:host host,
                :port port,
                :frame (string :utf-8 :delimiters ["\n"])})))

(defmulti handle-msg :msgType)

(defmethod handle-msg "carPositions" [msg]
  {:msgType "throttle" :data 0.5})

(defmethod handle-msg :default [msg]
  {:msgType "ping" :data "ping"})

(defn log-msg [msg]
  (case (:msgType msg)
    "join" (println "Joined")
    "gameStart" (println "Race started")
    "crash" (println "Someone crashed")
    "gameEnd" (println "Race ended")
    "error" (println (str "ERROR: " (:data msg)))
    :noop))

(defn game-loop [channel]
  (let [msg (read-message channel)]
    (log-msg msg)
    (send-message channel (handle-msg msg))
    (recur channel)))

(defn -main[& [host port botname botkey]]
  (let [channel (connect-client-channel host (Integer/parseInt port))]
    (send-message channel {:msgType "join" :data {:name botname :key botkey}})
    (game-loop channel)))
