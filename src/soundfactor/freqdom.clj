(ns soundfactor.freqdom
  (:require [quil.core :as q])
  (:require [soundfactor.command :as command])
  (:require [soundfactor.util :as util])
  (:require [soundfactor.mic :as mic])
  (:import [java.nio ByteBuffer ShortBuffer])
)

(defn draw [state]
  (let [[max-y fft]      @state
        screen-width     (q/width)
        screen-height    (q/height)
        freq-width       22000
        stroke-width     1
        x-scale          (/ screen-width freq-width)
        y-scale          (/ screen-height max-y)]
    (q/background 0) ; clear screen
    (q/stroke-weight stroke-width)
    (q/stroke 255)
    (printf "draw: x-scale: %f, y-scale: %f\n" (float x-scale) (float y-scale)) (flush)
    (doseq [i  (range freq-width)]
      (let [j  (max (aget fft i) 0)
            x  (int (* i x-scale))
            y  (int (* j y-scale))]
        (q/line x screen-height x (- screen-height y))))))

(defn tick [state]
  (let [mic-line (mic/open-mic-input-line (mic/get-mixer-with-the-mic))]
    (. mic-line start)
    (let [sample-size-in-bytes 2
          samples-per-second   1
          samples-needed       (/ 44100 samples-per-second)
          buffer-size          (* samples-needed sample-size-in-bytes)
          short-buffer         (short-array (/ buffer-size 2))
          buffer               (make-array (. Byte TYPE) buffer-size)
          bcount               (. mic-line (read buffer 0 buffer-size))
          bbyte                (. ByteBuffer (wrap buffer))
          bshort               (. bbyte (asShortBuffer))
          _ignored             (. bshort (get short-buffer))
          fft                  (util/compute-fft short-buffer)]
      (swap! state (fn [[max-y prev-fft]] 
                     (printf "tick! bcount: %d\n" bcount) (flush) 
                     [(max max-y (reduce max fft)) fft])))
    (Thread/sleep 1000)
    (. mic-line close)))

(defn main []
  (let [series-size  50000
        freq-series  (double-array series-size)
        state        (atom [1 freq-series])]
    (.start (Thread. (fn [] (while true (tick state)))))
    (q/defsketch freqdom
      :title "freqdom"
      :setup (fn []
               (q/smooth)
               (q/frame-rate 5)
               (q/background 0))
      :draw (partial draw state)
      :size [800 600])))

(def cmd 
  (command/basic :summary "display frequency domain of mic"
                 :spec []
                 :main main))
