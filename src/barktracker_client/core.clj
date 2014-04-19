(ns barktracker-client.core
  [:require [clj-http.client :as client]]
  [:import [javax.sound.sampled AudioFormat AudioSystem DataLine DataLine$Info LineUnavailableException TargetDataLine]
           [java.nio Buffer ByteBuffer ByteOrder]
           [java.io File]])

(defn rms [xs]
  (Math/sqrt (/ (reduce + (map #(* % %) xs))
           (count xs))))

(def audio-format (AudioFormat. 44100 8 1 true true))

(defn open-line [audio-format]
  (let [^TargetDataLine line (AudioSystem/getLine (DataLine$Info. TargetDataLine audio-format))]
    (doto line
      (.open audio-format)
      (.start))
    line))

(defn file-line [file]
  (AudioSystem/getAudioInputStream (File. file)))

(defn log-bark [start stop]
  (do
    (println (str "Start: " start ", stop: " stop))
    (try
      (client/post "http://localhost:3000/barks" {:form-params {:start start :stop stop} :throw-exceptions false})
      (catch Exception ex))))

(defn listen-for-barks [threshold line buffer]
  ;(let [line (open-line audio-format)
        ;buffer (byte-array (/ (.getBufferSize line) 5))]
    (loop [bark-start-time 0]
      (.read line buffer 0 (count buffer))
      (let [is-barking (> (rms buffer) threshold)
            was-barking (< 0 bark-start-time)]
        (cond
          (= was-barking is-barking) (recur bark-start-time)
          is-barking (recur (System/currentTimeMillis))
          :else (do (log-bark bark-start-time (System/currentTimeMillis)) (recur 0))))))

(defn listen-for-barks-mic [threshold]
  (let [line (open-line audio-format)
        buffer (byte-array (/ (.getBufferSize line) 5))]
    (listen-for-barks threshold line buffer)))

(defn listen-for-barks-file [threshold file]
  (let [line (file-line file)
        buffer (byte-array 1000)]
    (listen-for-barks threshold line buffer)))

(defn to-short-array [in-buffer]
  (let [out-buffer (short-array (/ (count in-buffer) 2))]
    (-> in-buffer ByteBuffer/wrap (.order ByteOrder/LITTLE_ENDIAN) .asShortBuffer (.get out-buffer))
    out-buffer))

(defn listen-for-barks-recording [threshold]
  (let [line (open-line audio-format)
        buffer (byte-array (/ (.getBufferSize line) 5))]
    (loop [bark-start-time 0]
      (.read line buffer 0 (count buffer))
      (let [is-barking (> (rms buffer) threshold)
            was-barking (< 0 bark-start-time)]
        (cond
          (= was-barking is-barking) (recur bark-start-time)
          is-barking (recur (System/currentTimeMillis))
          :else (do (log-bark bark-start-time (System/currentTimeMillis)) (recur 0)))))))

(defn -main []
  (listen-for-barks-mic 4))
