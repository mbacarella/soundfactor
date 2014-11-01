(ns soundfactor.quildemo
  (:require [quil.core :as q])
  (:require [soundfactor.command :as command])
)

(defn setup []
  (q/smooth)  ; anti-aliasing
  (q/frame-rate 1)
  (q/background 200))
                                      
(defn draw []
  (q/stroke (q/random 255))
  (q/stroke-weight (q/random 10))     
  (q/fill (q/random 255))
  (let [diam (q/random 100)
        x    (q/random (q/width))
        y    (q/random (q/height))]
    (q/ellipse x y diam diam)))

(defn gray-circles-demo []
  (q/defsketch gray-circles-demo
    :title "gray circles"
    :setup setup
    :draw draw
    :size [800 600]))

(def cmd (command/basic :summary "quil demo"
                        :spec []
                        :main gray-circles-demo))
