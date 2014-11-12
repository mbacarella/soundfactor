(ns soundfactor.visualize
  (:require [quil.core :as q])
  (:require [soundfactor.command :as command])
  (:require [soundfactor.util :as util])
  (:require [soundfactor.mic :as mic]))

(import '(java.nio ByteBuffer ShortBuffer))

(defn draw [state]
  (let [[pcm-series spectro-series time]    @state
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
            f  (aget spectro-series i')]
        (q/stroke-float (if (> f 16000) 255 0)
                        (if (and (<= f 16000) (> f 4000)) 255 0)
                        (if (<= f 4000) 255 0))
        (q/line x y x (+ y (/ h 2)))))))

(defn clamp-to-short [x] 
  (-> x (min 32767) (max -32766)))

(defn tick [mic-line state]
  (let [sample-size-in-bytes 2
        samples-per-second   100
        samples-needed       (/ 44100 samples-per-second)
        buffer-size          (* samples-needed sample-size-in-bytes)
        short-buffer         (short-array (/ buffer-size 2))
        buffer               (make-array (. Byte TYPE) buffer-size)
        bcount               (. mic-line (read buffer 0 buffer-size))
        bbyte                (. ByteBuffer (wrap buffer))
        bshort               (. bbyte (asShortBuffer))
        ; _ignored             (. bshort (get short-buffer))
        pcm-value            (reduce (fn [acc x] (if (> (Math/abs (float x)) (Math/abs acc))
                                                   (float x) acc))
                                     (float 0)
                                     short-buffer)
        fft                  (util/compute-fft short-buffer)
        spectro-value        (clamp-to-short (* (util/dominant-frequency fft) samples-per-second))]
    (swap! state (fn [[pcm-series spectro-series time]]
                   ;; this isn't actually an atomic update because series is a mutable array
                   ;; but it probably doesn't matter because we're using it as a circular buffer
                   (let [next-time (inc time)
                         index     (mod next-time (alength pcm-series))]
                     (aset pcm-series index (short pcm-value))
                     (aset spectro-series index (short spectro-value))
                     [pcm-series spectro-series next-time])))))

(defn main []
  (let [series-size    1000
        pcm-series     (short-array series-size)
        spectro-series (short-array series-size)
        time           0
        state          (atom [pcm-series spectro-series time])
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
