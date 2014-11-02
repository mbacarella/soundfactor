(ns soundfactor.visualize
  (:require [quil.core :as q])
  (:require [soundfactor.command :as command])
  (:require [soundfactor.util :as util])
;  (:require [javax.sound.sampled.AudioSystem :as AudioSystem])
;  (:require [javax.sound.sampled.AudioFormat :as AudioFormat])
;  (:require [javax.sound.sampled.TargetDataLine :as TargetDataLine])
)

;; htf do i rewrite this as a :require
(import '(java.nio ByteBuffer ShortBuffer))
(import '(javax.sound.sampled AudioFormat AudioSystem TargetDataLine))

;; microphone input setup

(def mic-input-format 
  (new AudioFormat 44100 16 1 true true)
  ; (new AudioFormat 44100 8 1 false true)
)

(def buffer-size 44100) ; why?

(defn get-all-mixers []
  (map #(let [m %] {:mixer-info m 
                    :name (. m (getName))
                    :description (. m (getDescription))})
       (seq (. AudioSystem (getMixerInfo)))))

(defn get-mixer-with-the-mic [] 
  (first (filter (fn [m] (= (m :name) "default [default]")) (get-all-mixers))))

(defn open-mic-input-line [mixer-info]
  (let [mic-mixer-info  (mixer-info :mixer-info)
        mic-mixer       (. AudioSystem (getMixer mic-mixer-info))
        _sources        (seq (. mic-mixer (getSourceLineInfo))) ; sources are written to
        targets         (seq (. mic-mixer (getTargetLineInfo))) ; targets are read from
        line-info       (first targets) ; more guessing
        mic-line        (. mic-mixer (getLine line-info))]
    ; (. mic-line (open mic-input-format buffer-size))
    (.open #^TargetDataLine mic-line mic-input-format buffer-size)
    mic-line))

;; graphics setup

(defn setup []
  (q/smooth)
  (q/frame-rate 24)
  (q/background 0))

(defn draw [state]
  (let [[series time]    @state
        screen-width     (q/width)
        screen-height    (q/height)
        samples-per-sec  30
        stroke-width     (/ screen-width samples-per-sec)]
    ;; draw bars starting on the right edge at time, then walk left/backwards
    ;; through time drawing history
    ;(printf "time: %d\n" time) (flush)
    (q/background 0) ; clear screen
    (q/stroke-weight stroke-width)
    (q/stroke 255)
    (doseq [i  (range time (- time samples-per-sec) -1)]
      (let [i' (mod i (alength series))
            x  (* stroke-width (- samples-per-sec (- time i)))
            y  (/ screen-height 2)
            m  (aget series i')
            m  (* m 75) ; ghetto boost, use something logarithmic before anyone finds out
            h  (* (/ m 32768) screen-height)]
        ;(printf "x: %d, y: %d, h: %d\n" x y (int h)) (flush)
        (q/line x y x (+ y (/ h 2)))))))

(defn tick [mic-line state]
  (let [sample-size-in-bytes 2
        samples-needed       (/ 44100 100)
        buffer-size          (* samples-needed sample-size-in-bytes)
        short-buffer         (short-array (/ buffer-size 2))
        buffer               (make-array (. Byte TYPE) buffer-size)
        bcount               (. mic-line (read buffer 0 buffer-size))
        bbyte                (. ByteBuffer (wrap buffer))
        bshort               (. bbyte (asShortBuffer))
        _ignored             (. bshort (get short-buffer))
        ;; compute summary of sample data
        mean                 (/ (apply + short-buffer) (alength short-buffer))]
    (swap! state (fn [[series time]]
                   ;; this isn't actually an atomic update because series is a mutable array
                   ;; but... it's the thought that counts right?
                   (let [next-time (inc time)]
                     (aset series (mod next-time (alength series)) (short mean))
                     [series next-time])))))

(defn main []
  (let [series     (short-array 1000)
        time       0
        state      (atom [series time])
        mic-line   (open-mic-input-line (get-mixer-with-the-mic))]
    (.start (Thread. (fn [] 
                       (. mic-line start) ; start reading the microphone
                       (while true (tick mic-line state)))))
    (q/defsketch visualize
      :title "visualize"
      :setup setup
      :draw (partial draw state)
      :size [800 600])))

(def cmd 
  (command/basic :summary "visualize"
                 :spec []
                 :main main))
