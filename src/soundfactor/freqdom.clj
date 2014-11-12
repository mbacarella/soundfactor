(ns soundfactor.freqdom
  (:require [quil.core :as q])
  (:require [soundfactor.command :as command])
  (:require [soundfactor.util :as util])
  (:require [soundfactor.mic :as mic]))

(import '(java.nio ByteBuffer ShortBuffer))

(defn draw [state]
  (let [fft              @state
        screen-width     (q/width)
        screen-height    (q/height)
        freq-width       22500
        stroke-width     1
        x-scale          (/ screen-width freq-width)
        y-scale          (/ screen-height (reduce max fft))]
    (q/background 0) ; clear screen
    (q/stroke-weight stroke-width)
    (q/stroke 255)
    (doseq [i  (range freq-width)]
      (let [j  (aget fft i)
            x  (int (* i x-scale))
            y  (int (* j y-scale))]
        (q/line x 0 x y)))))

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
          fft                  (util/compute-fft short-buffer)]
      (swap! state (fn [prev-fft] fft)))
    (Thread/sleep 1000)
    (. mic-line close)))

(defn main []
  (let [series-size  50000
        freq-series  (double-array series-size)
        state        (atom freq-series)]
    (.start (Thread. (fn [] (while true (tick state)))))
    (q/defsketch freqdom
      :title "freqdom"
      :setup (fn []
               (q/smooth)
               (q/frame-rate 24)
               (q/background 0))
      :draw (partial draw state)
      :size [800 600])))

(def cmd 
  (command/basic :summary "display frequency domain of mic"
                 :spec []
                 :main main))
