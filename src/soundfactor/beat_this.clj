;; This module is a translation of the beat-this algorithm as specified by
;; https://www.clear.rice.edu/elec301/Projects01/beat_sync/beatalgo.html
(ns soundfactor.beat-this
  (:gen-class)
  (:require [soundfactor.util :as util])
  )

(def fft util/fft)
(def ifft util/ifft)

(defn array-init [len f]
  (double-array len (map f (range len))))

;; XXX: you can't use subvec on arrays!?
;; XXX: you can't take efficient sub-arrays of arrays?!
(defn array-slice [a start end]
  (let [len (- end start)]
    (array-init len (fn [i] (aget a (+ start i))))))

;; -- begin matrix functions --

(defrecord CrappyMatrix [rows columns array])

(defn matrix-new [rows columns]
  (CrappyMatrix. rows columns (double-array (* rows columns) 0)))

;; XXX: test me
(defn matrix-splice [matrix first-row last-row col data]
  (doseq [i (range (- last-row first-row))]
    (let [index (+ col (* (+ first-row i) (:columns matrix)))]
      (aset-double (:array matrix) index (aget data i)))))

;; XXX: test me
(defn matrix-column [matrix col]
  (double-array (for [i (range (:rows matrix))]
                  (aget (:array matrix) (+ col (* i (:columns matrix)))))))

;; XXX: test me
(defn matrix-get [matrix row col]
  (aget (:array matrix) (+ col (* row (:columns matrix)))))

;; XXX: test me
(defn matrix-set [matrix row col val]
  (aset-double (:array matrix) (+ col (* row (:columns matrix))) val))

;; -- end matrix functions --

(def bandlimits 
  (let [bandlimits [0 200 400 800 1600 3200]]
    (int-array (count bandlimits) bandlimits)))
(def maxfreq 4096)
(def nbands (count bandlimits))

;; --- FILTER BANK ---

(defn filter-bank [sig]
  (let [dft        (fft sig)
        n          (count sig)
        ;; bring band scale from Hz to the points in our vectors
        bl         (array-init nbands (fn [i] (inc (Math/floor (/ (* (/ (aget bandlimits i) maxfreq) n) 2)))))
        br         (array-init nbands (fn [i] (if (= (dec nbands)) (Math/floor (/ n 2))
                                                  (Math/floor  (/ (* (/ (aget bandlimits (inc i)) maxfreq) n) 2)))))
        matrix     (matrix-new n nbands)]
    (doseq [i (range nbands)]
      (let [bl_i (aget bl i)
            br_i (aget br i)
            ;; XXX: i think the +1 isn't needed?
            np1  (inc n)
            np1_m_br_i (- np1 br_i)
            np1_m_bl_i (- np1 bl_i)]
        ;; output(bl(i):br(i),i) = dft(bl(i):br(i));
        (matrix-splice matrix bl_i br_i i (array-slice dft bl_i br_i))
        ;; output(n+1-br(i):n+1-bl(i),i) = dft(n+1-br(i):n+1-bl(i));
        (matrix-splice matrix np1_m_br_i np1_m_bl_i i (array-slice dft np1_m_br_i np1_m_bl_i))))
    (aset-double (:array matrix) 0 0.0)
    matrix))

;; --- HWINDOW ---  

;; TODO: understand what this all means  
;; what's a hanning window?  
;; what's a half-hanning window?  
;; why take the fourier transform of it?  
(def winlength 0.4)  ;; seconds
(def hannlen (* winlength 2 maxfreq))

(defn hwindow [fdsig]
  (let [n        (count fdsig)
        ;; Create half-Hanning window
        hann     (array-init hannlen (fn [a] (Math/pow (Math/cos (/ (* a Math/PI) hannlen 2)) 2)))
        hann-dft (fft hann)
        wave     (matrix-new n nbands)
        freq     (matrix-new n nbands)
        output   (matrix-new n nbands)
        ]  
    ;; take IFFT to transform to time domain
    (doseq [i (range nbands)]
      (matrix-splice wave 0 n i (double-array (ifft (matrix-column fdsig i)))))
    ;; Full-wave rectification in the time domain
    ;; And back to frequency with FFT.
    (doseq [i (range nbands)]
      (doseq [j (range n)]
        (let [wave_j_i (matrix-get wave j i)]
          (if (< wave_j_i 0)
            (matrix-set wave j i (- wave_j_i)))))
      (matrix-splice freq 0 n i (fft (matrix-column wave i))))
    ;; Convolving with half-Hanning same as multiplying in frequency.
    ;; Multiply half-Hanning FFT by signal FFT.  Inverse transform
    ;; to get output in the time domain.
    (doseq [i (range nbands)]
      (let [filtered (double-array (map * (matrix-column freq i) hann-dft))]
        (matrix-splice output 0 n i (ifft filtered))))
    output))

;; --- DIFFRECT ---

(defn diffrect [tdsig]
  (let [n      (count tdsig)
        output (matrix-new n nbands)]
    (doseq [i (range nbands)]
      (doseq [j (range 4 n)]
        ;; Find the difference from one sample to the next
        (let [d (- (matrix-get tdsig j i) (matrix-get tdsig (- j 1) i))]
          ;; Retain only if difference is positive (half-wave rectify)
          (if (> d 0) (matrix-set output j i d)))))
    output))

;; -- TIMECOMB --

(def minbpm 60)
(def maxbpm 240)

(defn convolve-one-band [dftfil dft]
  ;;  x = sum((abs(dftfil.*dft(:,i))).^2)
  (apply + (map (fn [a b] (Math/pow (Math/abs (* a b)) 2)) dftfil dft)))

(defn timecomb [sig acc]
  (let [n          (count sig)
        dft        (matrix-new n nbands)
        sbpm-maxe  (atom [-1 0]) ; initialize max energy to zero
        ]
    ;; get signal in frequency domain
    (doseq [i (range nbands)]
      (matrix-splice dft 0 n i (fft (matrix-column sig i))))
    ;; initialize max energy to zero
    (doseq [bpm (range minbpm maxbpm acc)]
      (let [nstep   (Math/floor (* (/ 120 bpm) maxfreq))
            ;; the cond below corresponds to a hard-coded 3 pulses in the comb filter
            fil     (array-init n (fn [i] (if (or (= i 0)
                                                  (= i nstep)
                                                  (= i (* nstep 2))) 1
                                                  0)))
            ;; get the filter in the frequency domain
            dftfil  (fft fil)
            ;; calculate the energy after convolution
            e       (apply + (map (fn [i] (convolve-one-band dftfil (matrix-column dft i)))
                                   (range nbands)))
            ]
        (swap! sbpm-maxe
               (fn [[sbpm maxe]] (if (> e maxe) [bpm e] [sbpm maxe])))))))
