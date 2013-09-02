(ns barktrack.core
  [:use [clojure.math [numeric-tower :only [sqrt]]]]
  [:use [clojure.data.json :as json]]
  [:require [clj-http.client :as client]]
  [:import (javax.sound.sampled AudioFormat AudioSystem DataLine DataLine$Info LineUnavailableException TargetDataLine)])

(defn rms [xs]
  (sqrt (/ (reduce + (map #(* % %) xs))
           (count xs))))

(def audio-format (AudioFormat. 44100 8 1 true true))

(defn open-line [audio-format]
  (let [^TargetDataLine line (AudioSystem/getLine (DataLine$Info. TargetDataLine audio-format))]
    (doto line
      (.open audio-format)
      (.start))
    line))

(defrecord Bark [start stop])

(def barks (ref ()))
(def stop-listening false)

(defn log-bark [bark]
  ;(client/post "http://google.com"
               ;{:body (json/write-str bark)
                ;:content-type :json}))

  (dosync (alter barks conj bark))
  (println @barks))

(defn listen-for-barks [threshold]
  (let [line (open-line audio-format)
        buffer (byte-array (/ (.getBufferSize line) 5))]
    (loop [bark-start-time 0]
      (.read line buffer 0 (count buffer))
      (let [is-barking (> (rms buffer) threshold)
            was-barking (< 0 bark-start-time)]
        (cond
          stop-listening @barks
          (= was-barking is-barking) (recur bark-start-time)
          is-barking (recur (System/currentTimeMillis))
          :else (do (log-bark (->Bark bark-start-time (System/currentTimeMillis))) (recur 0)))))))
          ;:else (do (log-bark {:start bark-start-time :stop (System/currentTimeMillis)}) (recur 0)))))))

;(def f
  ;(future
    ;(listen-for-barks 4)))

;(def stop-loop @f)

(defn -main []
  (do
    (listen-for-barks 4)
    (println @barks)))
