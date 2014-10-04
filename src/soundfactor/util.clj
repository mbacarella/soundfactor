
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

(defn get-mp3-sample-data-mono [mp3-file]
  "Return a short array of mp3 sample data"
  (let [mp3-decoder     "/usr/bin/mp3-decoder"
        my-byte-array   (:out (sh mp3-decoder "-m" "-s" mp3-file :out-enc :bytes))
        my-short-array  (short-array (/ (alength my-byte-array) 2))
        short-buffer    (.asShortBuffer (.order (ByteBuffer/wrap my-byte-array) ByteOrder/LITTLE_ENDIAN))]
    (do
      (.get short-buffer my-short-array)
      my-short-array)))

(defn dominant-frequency [fft-result n]
  (reduce (fn [[x-max i-max] [x i]]
            (if (> x x-max) [x i] [x-max i-max]))
          (map (fn [x i] [x i]) fft-result (range n))))

(defn peak-frequencies-of-mp3 [mp3-file freqs-per-sec]
  "Given an [mp3-file freqs-per-second], return a sequence of [seconds-from-start peak-frequency] values over the entire mp3"
  (let [samples-per-second  44100 ; mp3-decoder guarantees this
        bucket-size         (/ samples-per-second freqs-per-sec)
        mp3-raw-samples     (double-array (get-mp3-sample-data-mono mp3-file))
        num-mp3-raw-samples (alength mp3-raw-samples)
        total-buckets       (/ num-mp3-raw-samples bucket-size)
        fft                 (mikera.matrix.algo.FFT. bucket-size)
        tarr                (double-array (* bucket-size n 2))]
    (map (fn [i-bucket]
           (do
             (System/arraycopy mp3-raw-samples (* i-bucket bucket-size) tarr 0 bucket-size)
             (.realForward fft tarr)
             [(/ (* i-bucket bucket-size) samples-per-second) ; offset in seconds
              (dominant-frequency tarr bucket-size)]))
         (range total-buckets))))
