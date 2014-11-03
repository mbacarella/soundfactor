(ns soundfactor.gnuplot
  (:gen-class)
  (:require [soundfactor.util :as util])
  (:require [soundfactor.command :as command])
)

(defn write-pcm-and-spectro-dat [sample-span input-mp3 pcm-dat spectro-dat]
  (let [time-series       (util/get-mp3-sample-data-mono input-mp3)
        hz                44100
        samples-per-span  (int (* hz sample-span))
        spans-per-second  (/ 1 sample-span)
        parted-samples    (partition samples-per-span time-series)
        save-series       (fn [series path]
                            (util/write-lines path ; XXX: combine these into one map?
                                              (map (fn [[x y]] (str x " " y))
                                                   (map-indexed vector series))))]
    (save-series (map (fn [span-samples]
                        (util/mean span-samples samples-per-span))
                      parted-samples) pcm-dat)
    (save-series (map (fn [span-samples]
                        (* spans-per-second (util/dominant-frequency (util/compute-fft span-samples))))
                      parted-samples) spectro-dat)))

(def cmd
  (command/basic :summary "create pcm and spectrogram gnuplots from mp3s"
                 :spec [ (command/flag "-sample-span"
                                       (command/optional-with-default :float 0.1)
                                       :doc "<SPAN> sample-size")
                         (command/anon "input.mp3") ]
                 :main (fn [sample-span input-mp3]
                         (let [pid                 (util/getpid)
                               base                (format "/tmp/%s-%d" (util/basename input-mp3) pid)
                               tmpfile-pcm         (format "%s-pcm.gnuplot" base)
                               tmpfile-spectro     (format "%s-spectro.gnuplot" base)
                               tmpfile-pcm-dat     (format "%s-pcm.dat" base)
                               tmpfile-spectro-dat (format "%s-spectro.dat" base)]
                           (do
                             (printf "sample-span: %s\n" sample-span)
                             (printf "input-mp3: %s\n" input-mp3)
                             (printf "base: %s\n" base)
                             (flush)
                             (write-pcm-and-spectro-dat sample-span input-mp3 tmpfile-pcm-dat
                                                        tmpfile-spectro-dat)
                             (util/write-lines tmpfile-pcm
                                               ["reset"
                                                "set title \"pcm waveform\""
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
                                           "set yrange [0:24000]"
                                           "set term x11 1"
                                           (format "plot \"%s\"" tmpfile-spectro-dat)]))))))
