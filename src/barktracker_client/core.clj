(ns barktracker-client.core
  [:require [clj-http.client :as client]]
  [:import [javax.sound.sampled AudioFormat AudioSystem DataLine DataLine$Info LineUnavailableException TargetDataLine]
           [java.nio Buffer ByteBuffer ByteOrder]
           [java.io File]])

(use '(incanter core charts))
(def threshold 2000)
(def buffer-size 8820)
;(def buffer (short-array buffer-size))

(defn rms [xs]
  (Math/sqrt (/ (reduce + (map #(* % %) xs))
           (count xs))))

(def audio-format (AudioFormat. 44100 8 1 true false))

(defn open-line [audio-format]
  (let [^TargetDataLine line (AudioSystem/getLine (DataLine$Info. TargetDataLine audio-format))]
    (doto line
      (.open audio-format)
      (.start))
    line))

(defn file-line [file]
  (AudioSystem/getAudioInputStream (File. file)))

(defn to-short-array
  [in-buffer out-buffer]
    (-> in-buffer ByteBuffer/wrap (.order ByteOrder/LITTLE_ENDIAN) .asShortBuffer (.get out-buffer))
    out-buffer)

(defn read-into-buffer
  [line in-buffer out-buffer]
   (.read line in-buffer 0 buffer-size)
   (to-short-array in-buffer out-buffer)
   out-buffer)

(defn get-audio
  ([seconds]
   (let [line (open-line audio-format)]
     (get-audio line seconds)))
  ([line seconds]
   (let [in-buffer (byte-array buffer-size)
         out-buffer (short-array (/ buffer-size 2))]
     (flatten (repeatedly (* seconds 10) #(vec (read-into-buffer line in-buffer out-buffer)))))))

(defn chart-audio [seconds]
  (let [audio (get-audio seconds)]
    (view (xy-plot (range (count audio)) audio))))

(defn print-when-loud [audio]
  (if (> (rms audio) threshold) (println "Loud!")))

(defn microphone-stream [action]
  (let [in-buffer (byte-array buffer-size)
        out-buffer (short-array (/ buffer-size 2))
        line (open-line audio-format)]
    (doseq [audio-window (repeatedly #(read-into-buffer line in-buffer out-buffer))] (action audio-window))))

(defn read-entire-file [filename]
  (let [ais (file-line filename)
        byte-buffer (byte-array (.available ais))]
    (.read ais byte-buffer)
    (to-short-array byte-buffer)))

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

(defn read-from-line [line]
  (let [byte-buffer (byte-array buffer-size)]
    (.read line byte-buffer)
    (to-short-array byte-buffer)))

;(defn read-entire-line [line]
  ;(loop [data []]
    ;(cond
    ;(recur (conj data (read-from-line line)))))

;(defn listen-for-barks [line]
  ;(loop [bark-start-time 0]
    ;(let [buffer (read-from-line buffer)
          ;is-barking (> (rms buffer) threshold)]
         ;buffer (read-from-line line)]

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
