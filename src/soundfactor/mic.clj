(ns soundfactor.mic
  (:import [java.nio ByteBuffer ShortBuffer])
  (:import [javax.sound.sampled AudioFormat AudioSystem TargetDataLine])
)

(def mic-input-format 
  (new AudioFormat 44100 16 1 true true))

(def buffer-size 44100) ; why?

(defn get-all-mixers []
  (map #(let [m %] {:mixer-info m 
                    :name (. m (getName))
                    :description (. m (getDescription))})
       (seq (. AudioSystem (getMixerInfo)))))

(defn get-mixer-with-the-mic [] 
  ;; the name to use was determined experimentally
  (first (filter (fn [m] (= (m :name) "default [default]")) (get-all-mixers))))

(defn open-mic-input-line [mixer-info]
  (let [mic-mixer-info  (mixer-info :mixer-info)
        mic-mixer       (. AudioSystem (getMixer mic-mixer-info))
        ; _sources        (seq (. mic-mixer (getSourceLineInfo))) ; sources are written to
        targets         (seq (. mic-mixer (getTargetLineInfo))) ; targets are read from
        line-info       (first targets) ; more guessing
        mic-line        (. mic-mixer (getLine line-info))]
    (.open #^TargetDataLine mic-line mic-input-format buffer-size)
    mic-line))
