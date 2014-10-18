(ns soundfactor.core
  (:gen-class)
  (:import java.lang.Math)
  (:import java.io.File)
  (:import java.io.FileNotFoundException)
  (:import java.io.ByteArrayOutputStream)
  (:import java.nio.ByteBuffer)
  (:use [clojure.java.shell])
  (:use [soundfactor.command :as command])
  (:use [soundfactor.gnuplot :as gnuplot])
  (:use [soundfactor.util :as util])
)

;; (defn euclidean-norm [v]
;;   (Math/sqrt (reduce + (map (fn [x] (Math/pow x 2)) v))))

;; (defn dot-product [d1 d2] (reduce + (map * d1 d2)))

;; (defn cosine-similarity [d1 d2]
;;   (/ (dot-product d1 d2)
;;      (* (euclidean-norm d1)
;;         (euclidean-norm d2))))


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

(def samples-per-second 44100) ; mp3-decoder guarantees this?
(def buckets-per-second 30)    ; how many buckets/sec to build a spectrogram over

(defn get-peak-frequencies-of-mp3 [mp3-file]
  "Given an [mp3-file], return a sequence of [seconds-from-start peak-frequency] values over the entire mp3"
  (let [bucket-size         (/ samples-per-second buckets-per-second)
        mp3-raw-samples     (double-array (util/get-mp3-sample-data-mono mp3-file))
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
               [offset-in-seconds (* dom-freq buckets-per-second)])))
         (range total-buckets))))

(defn gen-freq-offset-mp3-map [freq-offset-mp3-map peak-frequencies mp3-file]
  "Turns a [offset freq] seq into a map of freq => [offset mp3] seq"
  (reduce
   (fn [freq-offset-mp3-map [offset peak-freq]]
     (let [key         peak-freq
           value       [offset mp3-file]]
       (assoc freq-offset-mp3-map key (conj (or (freq-offset-mp3-map key) #{}) value))))
   freq-offset-mp3-map
   peak-frequencies))

;; XXX: why doesn't this work?
(defn save-sexp [d f]
  (with-open [w (clojure.java.io/writer f)]
    (.write w (binding [*print-dup* true] (prn d)))))

(defn go [mp3-files]
  (reduce (fn [freq-offset-mp3-map mp3-file]
            (gen-freq-offset-mp3-map freq-offset-mp3-map
                                     (get-peak-frequencies-of-mp3 mp3-file)
                                     mp3-file))
          {}
          mp3-files))

(def cmd-test (command/basic :summary "enjoy music with more parts of your brain"
                           :spec [ (command/flag "-verbose" command/no-arg :doc "share more inner monologue")]
                           :main (fn [verbose]
                                   (printf "command.basic test\n")
                                   (printf "verbose: %s\n" verbose)
                                   (flush))))
(defn -main [& args] 
  (command/run
    (command/group "enjoy music with more parts of your brain"
                   [["test" cmd-test]
                    ["gnuplot" gnuplot/cmd]])))
