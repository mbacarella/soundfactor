;; This module is a translation of the beat-this algorithm as specified by
;; https://www.clear.rice.edu/elec301/Projects01/beat_sync/beatalgo.html
(ns soundfactor.beat-this
  (:gen-class)
  (:import [soundfactor.util :as util])
)

(def band-limits (apply vector [0 200 400 800 1600 3200]))
(def nbands      (count band-limits))
(def max-freq    4096)
(def floor       Math/floor)

(defn filter-bank [time-series]
  (let [data   (double-array time-series)
        n      (count data)
        dft    (util/fft data)  
        ;; bring band scale from Hz to the points in our vectors
        bl     (conj (map (fn [i] (inc (floor (/ (get band-limits i) (* max-freq n) 2))))
                          (range (dec n)))
                     (floor (inc (/ (get band-limits (dec nbands)) (* max-freq n) 2))))
        br     (conj (map (fn [i]  (floor (/ (get band-limits (inc i)) (* max-freq n) 2)))
                          (range (dec n)))
                     (floor (/ n 2)))
        output (zeroes n nbands)
    ;; create the frequency bands and put them in the vector output
    

)))
