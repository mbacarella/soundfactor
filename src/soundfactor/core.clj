(ns soundfactor.core
  (:gen-class)
  (:import java.lang.Math)
  (:import java.io.File)
  (:import java.io.FileNotFoundException)
  (:import java.io.ByteArrayOutputStream)
  (:import java.nio.ByteBuffer)
  (:import [java.nio ByteOrder])
  (:use [clojure.java.shell]))

;; (defn euclidean-norm [v]
;;   (Math/sqrt (reduce + (map (fn [x] (Math/pow x 2)) v))))

;; (defn dot-product [d1 d2] (reduce + (map * d1 d2)))

;; (defn cosine-similarity [d1 d2]
;;   (/ (dot-product d1 d2)
;;      (* (euclidean-norm d1)
;;         (euclidean-norm d2))))

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

;; (defn compare-one-mp3-to-rest [results-map first-mp3 rest-mp3s]
;;   (let [first-mp3-data (get-mp3-sample-data first-mp3)]
;;     (reduce (fn [results-map next-mp3]
;;               (let [test-key (sort [first-mp3 next-mp3])]
;;                 (if (not (nil? (results-map test-key)))
;;                   results-map ; we already did this test ??? just return
;;                   (try
;;                     (let [next-mp3-data  (get-mp3-sample-data next-mp3)]
;;                       (assoc results-map test-key [:ok (cosine-similarity first-mp3-data next-mp3-data)]))
;;                     (catch Exception e (assoc results-map test-key [:err e]))))))
;;             results-map rest-mp3s)))

;; (defn go [mp3s]
;;   (if (empty? mp3s)
;;     (printf "Usage: soundfactor <1.mp3> <2.mp3> [3.mp3] [...] [N.mp3\n")
;;     (let [map-of-results
;;       (loop [first-mp3   (first mp3s)
;;              rest-mp3s   (rest mp3s)
;;              results-map {}]
;;         (if (empty? rest-mp3s)
;;           results-map
;;           (recur (first rest-mp3s)
;;                  (rest  rest-mp3s)
;;                  (compare-one-mp3-to-rest results-map first-mp3 rest-mp3s))))]
;;       (doseq [[key [condition result]] map-of-results]
;;         (if (= condition :err)
;;           (printf "ERROR    %s\n" key)
;;           (printf "%.8f     %s\n" result key))
;;         (flush)))))

(defn get-mp3-sample-data-mono [mp3-file]
  "Return a short array of mp3 sample data"
  (let [mp3-decoder     "/usr/bin/mp3-decoder"
        my-byte-array   (:out (clojure.java.shell/sh mp3-decoder "-m" "-s" mp3-file :out-enc :bytes))
        my-short-array  (short-array (/ (alength my-byte-array) 2))
        short-buffer    (.asShortBuffer (.order (ByteBuffer/wrap my-byte-array) ByteOrder/LITTLE_ENDIAN))]
    (do
      (.get short-buffer my-short-array)
      my-short-array)))

(defn abs [n] (max n (- n)))

(defn dominant-frequency [fft-result n]
  (reduce (fn [[i-max x-max] [i x]]
            (let [abs-x (abs x)]
              (if (> abs-x x-max)
                [i abs-x]
                [i-max x-max])))
          (map-indexed vector fft-result)))

(def FREQUENCY-SAMPLES-PER-SECOND 30)

(defn get-peak-frequencies-of-mp3 [mp3-file nps]
  "Given an [mp3-file nps], return a sequence of [seconds-from-start peak-frequency] values over the entire mp3"
  (let [samples-per-second  44100 ; mp3-decoder guarantees this?
        bucket-size         (/ samples-per-second nps)
        mp3-raw-samples     (double-array (get-mp3-sample-data-mono mp3-file))
        num-mp3-raw-samples (alength mp3-raw-samples)
        total-buckets       (- (/ num-mp3-raw-samples bucket-size) 1) ; throw away the end bucket
        fft                 (mikera.matrixx.algo.FFT. (int bucket-size))
        tarr                (double-array (* bucket-size 2))]
    (map (fn [i-bucket]
           (do
             (System/arraycopy mp3-raw-samples (* i-bucket bucket-size) tarr 0 bucket-size)
             (.realForward fft tarr) ; magic
             (let [offset-in-seconds  (/ (* i-bucket bucket-size) (float samples-per-second))
                   [dom-freq _value]  (dominant-frequency tarr bucket-size)]
               [offset-in-seconds (* dom-freq nps)])))
         (range total-buckets))))

(defn go [mp3-files]
  (doseq [mp3-file mp3-files]
    (let [peak-frequencies (get-peak-frequencies-of-mp3 mp3-file FREQUENCY-SAMPLES-PER-SECOND)
          spectro-path     (str mp3-file ".spectrogram")]
      (with-open [writer (clojure.java.io/writer spectro-path)]
        (do
          (.write writer "# time peak-frequency\n")
          (doseq [[offset freq] peak-frequencies]
            (.write writer (str offset " " freq "\n"))))))))

(defn -main [& args] (go args))
