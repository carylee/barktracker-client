(import '(javax.sound.sampled AudioFormat AudioSystem DataLine DataLine$Info LineUnavailableException TargetDataLine))
(require '[clojure.math.numeric-tower :as math])

(defn rms [xs]
  (math/sqrt (/ (reduce + (map #(* % %) xs))
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

(defn log-bark [bark]
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
          (= was-barking is-barking) (recur bark-start-time)
          is-barking (recur (System/currentTimeMillis))
          :else (do (log-bark (->Bark bark-start-time (System/currentTimeMillis))) (recur 0)))))))

(listen-for-barks 4)
