(ns soundfactor.gnuplot
  (:gen-class)
  (:use [soundfactor.util :as util])
  (:use [soundfactor.command :as command])
)

;; (defn degrees-to-radians [degrees]
;;   (/ (* Math/PI degrees) 180))

;; (defn sine-wave [n]
;;   (map (fn [x]
;;          (* (-> (mod x 360) degrees-to-radians Math/sin) 32768))
;;        (range n)))

;; (defn fft-of-sine-wave [n]
;;   (let [raw-wave (double-array (sine-wave n))
;;         fft      (mikera.matrixx.algo.FFT. (int n))
;;         tarr     (double-array (* n 2))]
;;     (do
;;       (System/arraycopy raw-wave 0 tarr 0 n)
;;       (.realForward fft tarr)
;;       tarr)))

(defn fft-of-data [time-series]
  (let [n     (count time-series)
        fft   (mikera.matrixx.algo.FFT. (int n))
        tarr  (double-array (* n 2))]
    (do
      (System/arraycopy time-series 0 tarr 0 n)
      (.realForward fft tarr)
      tarr)))

(defn dominant-frequency [fft-result]
  (reduce (fn [[i-max x-max] [i x]]
            (let [abs-x (util/abs x)]
              (if (> abs-x x-max)
                [i abs-x]
                [i-max x-max])))
          (map-indexed vector fft-result)))

;; (defn graph-sine-wave-and-its-fft [n]
;;   (let [time-dom    (sine-wave n)
;;         freq-dom    (fft-of-sine-wave n)
;;         save-series (fn [series path]
;;                       (with-open [wr (clojure.java.io/writer path)]
;;                         (doseq [[x y] (map-indexed vector series)]
;;                           (.write wr (str x " " y "\n")))))]
;;     (do
;;       (save-series time-dom "/tmp/sine-wave.txt")
;;       (save-series freq-dom "/tmp/sine-wave-fft.txt"))))

;; (defn write-spectrogram-for-gnuplot [peak-frequencies spectro-path]
;;   (with-open [writer (clojure.java.io/writer spectro-path)]
;;     (.write writer "# time peak-frequency\n")
;;     (doseq [[offset freq] peak-frequencies]
;;       (.write writer (str offset " " freq "\n")))))

(defn write-pcm-and-spectro-dat [sample-span input-mp3 pcm-dat spectro-dat]
  (let [time-series       (util/get-mp3-sample-data-mono input-mp3)
        hz                44100
        samples-per-span  (int (* hz sample-span))
        parted-samples    (partition samples-per-span time-series)
        save-series       (fn [series path]
                            (util/write-lines path ; XXX: combine these into one map?
                                              (map (fn [x y] (str x " " y "\n"))
                                                   (map-indexed vector series))))]
    (do
      (save-series (map (fn [span-samples] (util/mean span-samples samples-per-span)) parted-samples) pcm-dat)
      (save-series (map (fn [span-samples] (dominant-frequency (fft-of-data span-samples))) parted-samples) spectro-dat))))

(def cmd
  (command/basic :summary "create pcm and spectrograms from mp3s"
                 :spec [ (command/flag "-sample-span"
                                       (command/optional-with-default :float 0.1)
                                       :doc "<SPAN> sample-size")
                         (command/anon "input.mp3") ]
                 :main (fn [sample-span input-mp3]  ;; XXX: include pid in tmpfile names
                         (let [pid                 (util/getpid)
                               tmpfile-pcm         (format "/tmp/%d.%s-pcm.gnuplot" pid input-mp3)
                               tmpfile-spectro     (format "/tmp/%d.%s-spectro.gnuplot" pid input-mp3)
                               tmpfile-pcm-dat     (format "/tmp/%d.%s-pcm.dat" pid input-mp3)
                               tmpfile-spectro-dat (format "/tmp/%d.%s-spectro.dat" pid input-mp3)]
                           (do
                             (util/write-lines tmpfile-pcm
                                               ["reset"
                                                "set title \"sine wave\""
                                                "set xlabel \"time\""
                                                "set ylabel \"power\""
                                                "set grid"
                                                "set term x11 0"
                                                (format "plot \"%s\" with lines" tmpfile-pcm-dat)])
                             (util/write-lines tmpfile-spectro
                                          ["reset"
                                           "set title \"peak frequencies\""
                                           "set xlabel \"freq\""
                                           "set ylabel \"magnitude\""
                                           "set grid"
                                           "set xrange [0:24000]"
                                           "set term x11 1"
                                           (format "plot \"%s\"" tmpfile-spectro-dat)])
                             (write-pcm-and-spectro-dat sample-span input-mp3 tmpfile-pcm-dat
                                                        tmpfile-spectro-dat))))))
