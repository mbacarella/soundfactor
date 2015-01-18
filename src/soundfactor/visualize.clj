(ns soundfactor.visualize
  (:require [quil.core :as q])
  (:require [soundfactor.command :as command])
  (:require [soundfactor.util :as util])
  (:require [soundfactor.mic :as mic])
  (:import [java.nio ByteBuffer ShortBuffer])
)

(def screen-dimensions [1366 768])
(def frequency-range        22500)
(def frame-rate                30)
(def hz                      1500)  ; num of sound buckets per second
(def samples-on-screen        100)

(def total-draws (atom 0))
(def millis-at-last-fps-print (atom (util/millis-since-epoch)))
(def frames-at-last-fps-print (atom 0))

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
                  (cond (< freq 2500)                        [r (cons mag g) b]
                        (and (>= freq 2500) (< freq 12000))  [r g (cons mag b)]
                        (>= freq 12000)                      [(cons mag r) g b]))
                [[] [] []]
                (map-indexed (fn [freq mag] [(* freq hz) 
                                             (max 0 (/ mag max-freq-mag))]) freqs))
        crunch-r (crunch r)
        crunch-g (crunch g)
        crunch-b (crunch b)]
    (cond (>= crunch-r crunch-g crunch-b) [crunch-r 0 0]
          (>= crunch-g crunch-r crunch-b) [0 crunch-g 0]
          (>= crunch-b crunch-r crunch-g) [0 0 crunch-b]
          :else [255 0 255])))

(defn maybe-print-fps []
  (swap! total-draws inc)
  (let [millis-span    1000
        millis-now     (util/millis-since-epoch)
        millis-elapsed (- millis-now @millis-at-last-fps-print)
        frames-elapsed (- @total-draws @frames-at-last-fps-print)
        fps            (float (/ frames-elapsed (/ millis-elapsed 1000)))]
    (if (> millis-elapsed millis-span)
      (do
        (printf "%d, %d; fps: %.2f\n" millis-elapsed frames-elapsed fps)
        (flush)
        (swap! frames-at-last-fps-print (fn [_x] @total-draws))
        (swap! millis-at-last-fps-print (fn [_x] millis-now))))))

(defn draw [state]
  (let [[pcm-series freq-series time max-freq-mag]    @state
        screen-width      (q/width)
        screen-height     (q/height)
        stroke-width      (/ screen-width samples-on-screen)
        start-i           time
        end-i             (- time samples-on-screen)]
    (q/background 0) ; clear screen
    (q/stroke-weight stroke-width)
    (q/stroke 255)
    (loop [i     start-i
           prev  nil]
      (let [i' (mod i (alength pcm-series))
            m  (aget pcm-series i')
            h  (* (/ m 32768) screen-height)
            x  (* stroke-width (- samples-on-screen (- time i)))
            y  (+ (/ screen-height 2) (/ h 2))
            f  (aget freq-series i')
            [r g b] (project-freqs-into-rgb f max-freq-mag)]
        (if (= i end-i) () ; end loop
            (do (if (not (nil? prev))
                  (let [[prev_x prev_y] prev]
                    (q/stroke r g b)
                    (q/line prev_x prev_y x y)))
                (recur (- i 1) [x y])))))
    (maybe-print-fps)))

(defn clamp-to-short [x] 
  (-> x (min 32767) (max -32766)))

(defn tick [mic-line state]
  (let [sample-size-in-bytes 2
        samples-needed       (/ 44100 hz)
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
                     (System/arraycopy fft 0 freqs 0 (/ frequency-range hz))
                     [pcm-series freq-series next-time (reduce max max-freq-mag (aget freq-series index))])))))

(defn main []
  (let [series-size    5000
        pcm-series     (short-array series-size)
        freq-series    (make-array Double/TYPE series-size (/ frequency-range hz))
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
               (q/frame-rate frame-rate)
               (q/background 0))
      :draw (partial draw state)
      :size screen-dimensions)))

(def cmd 
  (command/basic :summary "visualize"
                 :spec []
                 :main main))
