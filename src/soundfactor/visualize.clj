(ns soundfactor.visualize
  (:require [quil.core :as q])
  (:require [soundfactor.command :as command])
  (:require [soundfactor.util :as util])
  (:require [soundfactor.mic :as mic]))

(import '(java.nio ByteBuffer ShortBuffer))

(def frequency-range 22500)
(def samples-per-second 100)

;; frequency buckets:
;;  low:      0 ->  2500
;;  mid:   2500 -> 12000
;;  high: 12000 ->   inf

(defn crunch [lst] 
  (let [mean    (/ (apply + lst) (count lst))
        scaled  (* mean 255)
        boosted (+ scaled 100)]
    (int (min boosted 255))))

(defn project-freqs-into-rgb [freqs max-freq-mag]
  (let [[r g b]
        (reduce (fn [[r g b] [freq mag]]
                  (cond (< freq 2500)                        [(cons mag r) g b]
                        (and (>= freq 2500) (< freq 12000))  [r (cons mag g) b]
                        (>= freq 12000)                      [r g (cons mag b)]))
                [[] [] []]
                (map-indexed (fn [freq mag] [(* freq samples-per-second) 
                                             (max 0 (/ mag max-freq-mag))]) freqs))]
    [(crunch r)
     (crunch g) 
     (crunch b)]))

(defn draw [state]
  (let [[pcm-series freq-series time max-freq-mag]    @state
        screen-width      (q/width)
        screen-height     (q/height)
        samples-per-sec   30
        stroke-width      (/ screen-width samples-per-sec)]
    (q/background 0) ; clear screen
    (q/stroke-weight stroke-width)
    (q/stroke 255)
    (doseq [i  (range time (- time samples-per-sec) -1)]
      (let [i' (mod i (alength pcm-series))
            x  (* stroke-width (- samples-per-sec (- time i)))
            y  (/ screen-height 2)
            m  (aget pcm-series i')
            h  (* (/ m 32768) screen-height)
            f  (aget freq-series i')
            [r g b] (project-freqs-into-rgb f max-freq-mag)]
        (printf "r: %d, g: %d, b: %d\n" r g b) (flush)
        (q/stroke r g b)
        (q/line x y x (+ y (/ h 2)))))))

(defn clamp-to-short [x] 
  (-> x (min 32767) (max -32766)))


(defn tick [mic-line state]
  (let [sample-size-in-bytes 2
        samples-needed       (/ 44100 samples-per-second)
        buffer-size          (* samples-needed sample-size-in-bytes)
        short-buffer         (short-array (/ buffer-size 2))
        buffer               (make-array (. Byte TYPE) buffer-size)
        bcount               (. mic-line (read buffer 0 buffer-size))
        bbyte                (. ByteBuffer (wrap buffer))
        bshort               (. bbyte (asShortBuffer))
        _ignored             (. bshort (get short-buffer))
        pcm-value            (reduce (fn [a x] (if (> (Math/abs a) (Math/abs (float x))) a (float x)))
                                     (float 0)
                                     short-buffer)
        fft                  (util/compute-fft short-buffer)]
    (swap! state (fn [[pcm-series freq-series time max-freq-mag]]
                   ;; this isn't actually an atomic update because series is a mutable array
                   ;; but it probably doesn't matter because we're using it as a circular buffer
                   (let [next-time (inc time)
                         index     (mod next-time (alength pcm-series))
                         freqs     (aget freq-series index)]
                     (aset pcm-series index (short pcm-value))
                     (System/arraycopy fft 0 freqs 0 (/ frequency-range samples-per-second))
                     [pcm-series freq-series next-time (reduce max max-freq-mag (aget freq-series index))])))))

(defn main []
  (let [series-size    1000
        pcm-series     (short-array series-size)
        freq-series    (make-array Double/TYPE series-size (/ frequency-range samples-per-second))
        time           0
        state          (atom [pcm-series freq-series time 1])
        mic-line       (mic/open-mic-input-line (mic/get-mixer-with-the-mic))]
    (.start (Thread. (fn [] 
                       (. mic-line start) ; start reading the microphone
                       (while true (tick mic-line state)))))
    (q/defsketch visualize
      :title "visualize"
      :setup (fn []
               (q/smooth)
               (q/frame-rate 24)
               (q/background 0))
      :draw (partial draw state)
      :size [800 600])))

(def cmd 
  (command/basic :summary "visualize"
                 :spec []
                 :main main))
