(ns soundfactor.util
  (:import [java.nio ByteBuffer ByteOrder])
  (:import java.util.Date)
  (:use [clojure.java.io])
  (:use [clojure.java.shell])
)

(defn mean [xs n] (float (/ (reduce + xs) n)))

(defn basename [path]
  (.getName (clojure.java.io/as-file path)))

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

;; TODO: fall back to the crappy java API version if we can't find mp3-decoder?
(defn get-mp3-sample-data-mono [mp3-file]
  "Return a short array of mp3 sample data"
  (if (not (.exists (clojure.java.io/as-file mp3-file)))
    (throw (Exception. (format "get-mp3-sample-data-mono: %s does not exist" mp3-file)))
    (let [mp3-decoder     "/usr/bin/mp3-decoder"
          my-byte-array   ((clojure.java.shell/sh mp3-decoder "-m" "-s" mp3-file :out-enc :bytes) :out)
          my-short-array  (short-array (/ (alength my-byte-array) 2))
          short-buffer    (.asShortBuffer (.order (ByteBuffer/wrap my-byte-array) ByteOrder/LITTLE_ENDIAN))]
      (do
        (.get short-buffer my-short-array)
        my-short-array))))

;; XXX: factor out fft and ifft
(defn fft [#^doubles sig]
  (let [n    (count sig)
        fft  (mikera.matrixx.algo.FFT. (int n))
        tarr (double-array (* n 2))]
    (System/arraycopy sig 0 tarr 0 n)
    (.realForward fft tarr)
    tarr))

(defn ifft [#^doubles sig]
  (let [n    (count sig)
        fft  (mikera.matrixx.algo.FFT. (int n))
        tarr (double-array (* n 2))]
    (System/arraycopy sig 0 tarr 0 n)
    (.realInverse fft tarr false) ; the boolean is whether or not we want scaling
    tarr))

(defn dominant-frequency [fft-result]
  (first
   (reduce (fn [[best-freq best-mag] [freq mag]]
             (if (> mag best-mag) [freq mag] [best-freq best-mag]))
           (map-indexed vector fft-result))))

(defn millis-since-epoch []
  (.getTime (Date.)))
