(ns soundfactor.util
  (:import java.nio.ByteBuffer)
  (:import java.nio.ByteOrder)
  (:use [clojure.java.io])
  (:use [clojure.java.shell])
)

;; XXX: there must be standard lib versions of these

(defn mean [xs n] (/ (reduce + xs) n))

(defn abs [n] (max n (- n)))

(defn write-lines [path lines]
  (with-open [writer (clojure.java.io/writer path)]
    (doseq [line lines]
      (.write writer line)
      (.write writer "\n"))))

(defn getpid []
  (let [pid-at-host-str
        (. (java.lang.management.ManagementFactory/getRuntimeMXBean) getName)]
    (if-let [match (re-find #"^(\d+)@" pid-at-host-str)]
      (read-string (second match)))))

;; XXX: fall back to the crappy java API version if we can't find mp3-decoder?
(defn get-mp3-sample-data-mono [mp3-file]
  "Return a short array of mp3 sample data"
  (let [mp3-decoder     "/usr/bin/mp3-decoder"
        my-byte-array   (:out (clojure.java.shell/sh mp3-decoder "-m" "-s" mp3-file :out-enc :bytes))
        my-short-array  (short-array (/ (alength my-byte-array) 2))
        short-buffer    (.asShortBuffer (.order (ByteBuffer/wrap my-byte-array) ByteOrder/LITTLE_ENDIAN))]
    (do
      (.get short-buffer my-short-array)
      my-short-array)))

;; (defn get-mp3-sample-data-native [mp3-file] 
;;   (try
;;     (let [file-object      (File. mp3-file)
;;           audio-in         (AudioSystem/getAudioInputStream file-object)
;;           base-format      (.getFormat audio-in)
;;           decoded-format   (let [channels    (.getChannels base-format)
;;                                  sample-rate (.getSampleRate base-format)]
;;                              (AudioFormat. AudioFormat$Encoding/PCM_SIGNED  ; encoding
;;                                            sample-rate    ; sample rate of base format 
;;                                            16             ; sample size in bits
;;                                            channels
;;                                            (* channels 2) ; frame size
;;                                            sample-rate    ; frame rate
;;                                            false          ; big endian
;;                                            ))
;;           line-info        (DataLine$Info. SourceDataLine decoded-format)
;;           decoded-stream   (AudioSystem/getAudioInputStream decoded-format audio-in)
;;           read-buffer      (byte-array 8192)
;;           ba-output-stream (ByteArrayOutputStream.)
;;           moar-data        (fn [] (.read decoded-stream read-buffer))]
;;       (loop [num-bytes (moar-data)]
;;         (if (> num-bytes 0)
;;           (do (.write ba-output-stream read-buffer 0 num-bytes)
;;               (recur (moar-data)))
;;           (let [total-bytes    (.size ba-output-stream)
;;                 num-samples    (/ total-bytes 2)
;;                 byte-buffer    (ByteBuffer/wrap (.toByteArray ba-output-stream) 0 total-bytes)
;;                 short-buffer   (.asShortBuffer byte-buffer)
;;                 my-short-array (short-array num-samples)]
;;             (do (.get short-buffer my-short-array 0 num-samples)
;;                 (.close decoded-stream)
;;                 (.close audio-in)
;;                 my-short-array)))))
;;     (catch Exception _e nil)))
