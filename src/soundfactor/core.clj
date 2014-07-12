(ns soundfactor.core
  (:gen-class)
  (:import java.lang.Math)
  (:import java.io.File)
  (:import java.io.FileNotFoundException)
  (:import java.io.ByteArrayOutputStream)
  (:import java.nio.ByteBuffer)
  (:import [javax.sound.sampled AudioSystem AudioFormat
            AudioFormat$Encoding DataLine$Info SourceDataLine]))

;; computing cosine similarity
(defn euclidean-norm [v]
  (Math/sqrt (reduce + (map (fn [x] (Math/pow x 2)) v))))

(defn dot-product [d1 d2] (reduce + (map * d1 d2)))

(defn cosine-similarity [d1 d2]
  (/ (dot-product d1 d2)
     (* (euclidean-norm d1)
        (euclidean-norm d2))))

(defn get-mp3-sample-data [mp3-file]
  (let [file-object      (File. mp3-file)
        audio-in         (AudioSystem/getAudioInputStream file-object)
        base-format      (.getFormat audio-in)
        decoded-format   (let [channels    (.getChannels base-format)
                               sample-rate (.getSampleRate base-format)]
                           (AudioFormat. AudioFormat$Encoding/PCM_SIGNED  ; encoding
                                         sample-rate    ; sample rate of base format 
                                         16             ; sample size in bits
                                         channels       ; mono is easier to analyze
                                         (* channels 2) ; frame size
                                         sample-rate    ; frame rate
                                         false          ; big endian
                                         ))
        line-info        (DataLine$Info. SourceDataLine decoded-format)
        decoded-stream   (AudioSystem/getAudioInputStream decoded-format audio-in)
        read-buffer      (byte-array 8192)
        ba-output-stream (ByteArrayOutputStream.)
        moar-data        (fn [] (.read decoded-stream read-buffer))]
    (loop [num-bytes (moar-data)]
      (if (> num-bytes 0)
        (do (.write ba-output-stream read-buffer 0 num-bytes)
            (recur (moar-data)))
        (let [total-bytes    (.size ba-output-stream)
              num-samples    (/ total-bytes 2)
              byte-buffer    (ByteBuffer/wrap (.toByteArray ba-output-stream) 0 total-bytes)
              short-buffer   (.asShortBuffer byte-buffer)
              my-short-array (short-array num-samples)]
          (do (.get short-buffer my-short-array 0 num-samples)
              (.close decoded-stream)
              (.close audio-in)
              my-short-array))))))

(defn print-mp3-similarities [mp3s]
  (loop [current-mp3    (first mp3s)
         remaining-mp3s (rest mp3s)]
    (if (nil? current-mp3) () ; done
        (do
          (printf "* Analyzing %s:\n" current-mp3)
          (flush)
          (let [current-mp3-data (get-mp3-sample-data current-mp3)]      
            (doseq [test-mp3 remaining-mp3s]
              (let [test-mp3-data (get-mp3-sample-data test-mp3)
                    measure       (cosine-similarity current-mp3-data test-mp3-data)]
                (do (printf "  *  %.4f: %s\n" measure test-mp3)
                    (flush)))))
          (recur (first remaining-mp3s)
                 (rest remaining-mp3s))))))

(defn go [all-mp3s]
  (if (empty? paths)
    (printf "Usage: soundfactor <1.mp3> <2.mp3> [3.mp3] [...] [N.mp3\n")
    (print-mp3-similarities all-mp3s)))

(defn -main [& args] (go args))
