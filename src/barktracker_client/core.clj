(ns barktracker-client.core
  [:require [clj-http.client :as client]]
  [:import [javax.sound.sampled AudioFormat AudioSystem DataLine DataLine$Info LineUnavailableException TargetDataLine]
           [java.nio Buffer ByteBuffer ByteOrder]
           [java.io File]])

;(use '(incanter core charts))
(def threshold 2500)
(def buffer-size 8820)

(defn rms [xs]
  (Math/sqrt (/ (reduce + (map #(* % %) xs))
           (count xs))))

(def audio-format (AudioFormat. 44100 16 1 true false))

(defn mic-line [audio-format]
  (let [^TargetDataLine line (AudioSystem/getLine (DataLine$Info. TargetDataLine audio-format))]
    (doto line
      (.open audio-format)
      (.start))
    line))

(defn file-line [file]
  (AudioSystem/getAudioInputStream (File. file)))

(defn to-short-array [in-buffer out-buffer]
    (-> in-buffer ByteBuffer/wrap (.order ByteOrder/LITTLE_ENDIAN) .asShortBuffer (.get out-buffer))
    out-buffer)

(defn read-into-buffer [line in-buffer out-buffer]
   (.read line in-buffer 0 buffer-size)
   (to-short-array in-buffer out-buffer)
   out-buffer)

(defn get-audio
  ([seconds]
   (let [line (mic-line audio-format)]
     (get-audio line seconds)))
  ([line seconds]
   (let [in-buffer (byte-array buffer-size)
         out-buffer (short-array (/ buffer-size 2))]
     (flatten (repeatedly (* seconds 10) #(vec (read-into-buffer line in-buffer out-buffer)))))))

;(defn chart-audio [seconds]
  ;(let [audio (get-audio seconds)]
    ;(view (xy-plot (range (count audio)) audio))))

(defn log-bark [pad length start]
  (do
    (println (str "Silence: " pad ", bark: " length " at " start))
    (try
      (client/post "http://localhost:3000/barks" {:form-params {:pad pad :length length :start start} :throw-exceptions false})
      (catch Exception ex))))

(defn listen-for-barks
  ([] (listen-for-barks (mic-line audio-format)))
  ([line]
    (let [in-buffer (byte-array buffer-size)
          out-buffer (short-array (/ buffer-size 2))]
      (loop [bark-start-time 0
             silence-duration 0
             bark-duration 0]
        (let [audio (read-into-buffer line in-buffer out-buffer)
              is-barking (> (rms audio) threshold)
              was-barking (> bark-duration 0)]
          (cond
            (= true was-barking is-barking) (recur bark-start-time silence-duration (inc bark-duration))
            (= false was-barking is-barking) (recur 0 (inc silence-duration) 0)
            (true? is-barking) (recur (System/currentTimeMillis) silence-duration 1)
            (true? was-barking) (do
                                  (log-bark silence-duration bark-duration bark-start-time)
                                  (recur 0 0 0))))))))

(defn -main []
  (listen-for-barks))
