;; This module is a translation of the beat-this algorithm as specified by
;; https://www.clear.rice.edu/elec301/Projects01/beat_sync/beatalgo.html
(ns soundfactor.beat-this
  (:gen-class)
)

(def band-limits [0 200 400 800 1600 3200])
(def max-freq    4096)

(defn filter-bank [time-series]
  (let [data   (double-array time-series)
        n      (count data)
        dft    (do (let [fft  (mikera.matrixx.algo.FFT. (int n))
                         tarr (double-array (* n 2))]
                     (System/arraycopy data 0 tarr 0 n)
                     (.realForward fft tarr)
                     tarr))
        floor  (Math/floor)
        nbands (count band-limits)
        ;; bring band scale from Hz to the points in our vectors
        bl     (conj (map (fn [i] (inc (floor (/ (aget band-limits i) (* max-freq n) 2))))
                          (range (dec n)))
                     (floor (inc (/ (aget band-limits (dec nbands)) (* max-freq n) 2))))
        br     (conj (map (fn [i]  (floor (/ (aget band-limits (inc i)) (* max-freq n) 2)))
                          (range (dec n)))
                     (floor (/ n 2)))
        output (zeroes n nbands)
        ]
    ;; create the frequency bands and put them in the vector output
    

))
