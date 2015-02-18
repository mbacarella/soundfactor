;; This module is a translation of the beat-this algorithm as specified by
;; https://www.clear.rice.edu/elec301/Projects01/beat_sync/beatalgo.html
(ns soundfactor.beat-this
  (:gen-class)
  (:import [soundfactor.util :as util])
)

(defn array-init [len f]
  (float-array len (map f (range len))))

(defrecord CrappyMatrix [rows columns array])

(defn matrix-new [rows columns]
  (CrappyMatrix. rows columns (float-array (* rows columns) 0)))

;; XXX: test me
(defn matrix-splice [matrix first-row last-row col data]
  (doseq [i (range (- last-row first-row))]
    (let [index (+ col (* (+ first-row i) (:columns matrix)))]
      (aset (:array matrix) index (aget data i)))))
   
(defn slice [array start end]
  (subvec array start end))

(def bandlimits 
  (let [bandlimits [0 200 400 800 1600 3200]]
    (int-array (count bandlimits) bandlimits)))
(def maxfreq 4096)
(def floor Math/floor)
(def nbands (count bandlimits))

(defn filter-bank [sig]
  (let [dft        (fft sig)
        n          (count dft)
        nbands     (count bandlimits)
        ;; bring band scale from Hz to the points in our vectors
        bl         (array-init nbands (fn [i] (inc (floor (/ (* (/ (aget bandlimits i) maxfreq) n))))))
        br         (array-init nbands (fn [i] (if (= (dec nbands)) (floor (/ n 2))
                                                  (floor (/ (* (/ (aget bandlimits (inc i)) maxfreq) n) 2)]))
        output     (matrix-new n nbands))]
    (doseq [i (range nbands)]
      (let [bl_i (aget bl i)
            br_i (aget br i)
            ;; XXX: i think the +1 isn't needed?
            np1  (inc n)
            np1_m_br_i (- np1 br_i)
            np1_m_bl_i (- np1 bl_i)]
        (matrix-splice output bl_i br_i i (slice dft bl_i br_i))
        (matrix-splice output np1_m_br_i np1_m_bl_i i (slice dft np1_br_i np1_m_bl_i))))
    (aset (:array matrix) 0 0)
    matrix))
