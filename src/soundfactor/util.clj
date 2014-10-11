(ns soundfactor.util
  (:gen-class)
  (:import java.lang.Math)
  (:import java.io.File)
  (:import java.io.FileNotFoundException)
  (:import java.io.ByteArrayOutputStream)
  (:import java.nio.ByteBuffer)
  (:import [java.nio ByteOrder])
  (:use [clojure.java.io])
  (:use [clojure.java.shell]))

(defn degrees-to-radians [degrees]
  (/ (* Math/PI degrees) 180))

(defn sine-wave [n]
  (map (fn [x]
         (* (-> (mod x 360) degrees-to-radians Math/sin) 32768))
       (range n)))

(defn fft-of-sine-wave [n]
  (let [raw-wave (double-array (sine-wave n))
        fft      (mikera.matrixx.algo.FFT. (int n))
        tarr     (double-array (* n 2))]
    (do
      (System/arraycopy raw-wave 0 tarr 0 n)
      (.realForward fft tarr)
      tarr)))

(defn get-mp3-sample-data-mono [mp3-file]
  "Return a short array of mp3 sample data"
  (let [mp3-decoder     "/usr/bin/mp3-decoder"
        my-byte-array   (:out (clojure.java.shell/sh mp3-decoder "-m" "-s" mp3-file :out-enc :bytes))
        my-short-array  (short-array (/ (alength my-byte-array) 2))
        short-buffer    (.asShortBuffer (.order (ByteBuffer/wrap my-byte-array) ByteOrder/LITTLE_ENDIAN))]
    (do
      (.get short-buffer my-short-array)
      my-short-array)))

(defn fft-of-data [sample-data n]
  (let [fft   (mikera.matrixx.algo.FFT. (int n))
        tarr  (double-array (* n 2))]
    (do
      (System/arraycopy sample-data 0 tarr 0 n)
      (.realForward fft tarr)
      tarr)))

(defn graph-sine-wave-and-its-fft [n]
  (let [time-dom    (sine-wave n)
        freq-dom    (fft-of-sine-wave n)
        save-series (fn [series path]
                      (with-open [wr (clojure.java.io/writer path)]
                        (doseq [[x y] (map-indexed vector series)]
                          (.write wr (str x " " y "\n")))))]
    (do
      (save-series time-dom "/tmp/sine-wave.txt")
      (save-series freq-dom "/tmp/sine-wave-fft.txt"))))

(defn graph-mp3-and-its-fft [path n]
  (let [time-dom    (double-array (take n (drop 100000 (get-mp3-sample-data-mono path))))
        freq-dom    (fft-of-data time-dom n)
        save-series (fn [series path]
                      (with-open [wr (clojure.java.io/writer path)]
                        (doseq [[x y] (map-indexed vector series)]
                          (.write wr (str x " " y "\n")))))]
    (do
      (save-series time-dom "/tmp/mp3-wave.txt")
      (save-series freq-dom "/tmp/mp3-wave-fft.txt"))))

(defn write-spectrogram-for-gnuplot [peak-frequencies spectro-path]
  (with-open [writer (clojure.java.io/writer spectro-path)]
    (.write writer "# time peak-frequency\n")
    (doseq [[offset freq] peak-frequencies]
      (.write writer (str offset " " freq "\n")))))

;; ;; gnuplot commands to render the above
;; reset
;; set title "sine wave"
;; set xlabel "time"
;; set ylabel "power"
;; set grid
;; set term x11 0
;; plot "/tmp/mp3-wave.txt" title "" with lines

;; reset
;; set title "dft sine wave"
;; set xlabel "freq"
;; set ylabel "magnitude"
;; set grid
;; set xrange [0:24000]
;; set term x11 1
;; plot "/tmp/mp3-wave-fft.txt" title "" with lines
