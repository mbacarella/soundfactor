;; This module is a translation of the beat-this algorithm as specified by
;; https://www.clear.rice.edu/elec301/Projects01/beat_sync/beatalgo.html
(ns soundfactor.beat-this
  (:gen-class)
  (:import [soundfactor.util :as util])
)

(def fft util/fft)

(defn array-init [len f]
  (float-array len (map f (range len))))

;; move these into util

(defrecord CrappyMatrix [rows columns array])

(defn matrix-new [rows columns]
  (CrappyMatrix. rows columns (float-array (* rows columns) 0)))

;; XXX: test me
(defn matrix-splice [matrix first-row last-row col data]
  (doseq [i (range (- last-row first-row))]
    (let [index (+ col (* (+ first-row i) (:columns matrix)))]
      (aset (:array matrix) index (aget data i)))))

(def bandlimits 
  (let [bandlimits [0 200 400 800 1600 3200]]
    (int-array (count bandlimits) bandlimits)))
(def maxfreq 4096)
(def floor Math/floor)
(def nbands (count bandlimits))

;; --- FILTER BANK ---

(defn filter-bank [sig]
  (let [dft        (fft sig)
        n          (count sig)
        ;; bring band scale from Hz to the points in our vectors
        bl         (array-init nbands (fn [i] (inc (floor (/ (* (/ (aget bandlimits i) maxfreq) n))))))
        br         (array-init nbands (fn [i] (if (= (dec nbands)) (floor (/ n 2))
                                                  (floor (/ (* (/ (aget bandlimits (inc i)) maxfreq) n) 2)))))
        matrix     (matrix-new n nbands)]
    (doseq [i (range nbands)]
      (let [bl_i (aget bl i)
            br_i (aget br i)
            ;; XXX: i think the +1 isn't needed?
            np1  (inc n)
            np1_m_br_i (- np1 br_i)
            np1_m_bl_i (- np1 bl_i)]
        ;; output(bl(i):br(i),i) = dft(bl(i):br(i));
        (matrix-splice matrix bl_i br_i i (subvec dft bl_i br_i))
        ;; output(n+1-br(i):n+1-bl(i),i) = dft(n+1-br(i):n+1-bl(i));
        (matrix-splice matrix np1_m_br_i np1_m_bl_i i (subvec dft np1_m_br_i np1_m_bl_i))))
    (aset (:array matrix) 0 0)
    matrix))

;; --- HWINDOW ---  
  
;; TODO: understand what this all means  
;; what's a hanning window?  
;; what's a half-hanning window?  
;; why take the fourier transform of it?  
(defn hwindow [fdsig]  
  (let [n        (count fdsig)  
        ;; Create half-Hanning window  
        hann     (array-init hannlen (fn [a] (Math/pow (cos (/ (* a pi) hannlen 2)) 2)))  
        hann-dft (fft hann)  
        wave     (matrix-new n nbands)  
        freq     (matrix-new n nbands)  
        output   (matrix-new n nbands)]  
    ;; take IFFT to transform to time domain  
    (doseq [i (range nbands)]  
      (matrix-splice wave 0 n i (map real (map ifft (matrix-column fdsig i)))))  
    ;; Full-wave rectification in the time domain  
    ;; And back to frequency with FFT.  
    (doseq [i (range nbands)]  
      (doseq [j (range n)]  
        (let [wave_j_i (matrix-get wave j i)]  
          (if (< wave_j_i 0)  
            (matrix-set wave j i (- wave_j_i)))))
      (matrix-splice freq 0 n (fft (matrix-column wave i))))
    ;; Convolving with half-Hanning same as multiplying in frequency.
    ;; Multiply half-Hanning FFT by signal FFT.  Inverse transform
    ;; to get output in the time domain.
    (doseq [i (range nbands)]
      (let [filtered (map * (matrix-column freq i) hann-dft)]
        (matrix-splice output 0 n i (real (ifft filtered)))))
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
  (apply + (map (fn [a b] (pow (abs (* a b))) 2) dftfil dft)))

(defn timecomb [sig acc]
  (let [n          (count sig)
        npulses    3  ; number of pulses in the comb filter
        dft        (matrix-new n nbands)
        sbpm-maxe  (atom [-1 0]) ; initialize max energy to zero
        ]
    ;; get signal in frequency domain
    (doseq [i (range nbands)]
      (matrix-splice dft 0 n i (fft (matrix-column sig i))))
    ;; initialize max energy to zero
    (doseq [bpm (range minbpm maxbpm acc)]
      (let [nstep   (floor (* (/ 120 bpm) maxfreq))
            ;; set every nstep samples of the filter to 1
            fil     (array-init n (fn [i] (if (= i (* a nstep)) 1 0)))
            ;; get the filter in the frequency domain
            dftfil  (fft fil)
            ;; calculate the energy after convolution
            e       (apply + (map (fn [i] (convolve-one-band dftfil (matrix-column dft i)))
                                   (range nbands)))
            ]
        (swap! sbpm-maxe
               (fn [[sbpm maxe]] (if (> e maxe) [bpm e] [sbpm maxe])))))))
