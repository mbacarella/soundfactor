(ns soundfactor.core
  (:gen-class)
  (:import java.lang.Math)
  (:use [clojure.java.shell])
  (:require [soundfactor.command :as command])
  (:require [soundfactor.gnuplot :as gnuplot])
  (:require [soundfactor.quildemo :as quildemo])
  (:require [soundfactor.util :as util])
  (:require [soundfactor.visualize :as visualize])
  (:require [soundfactor.freqdemo :as freqdemo]))

(defn -main [& args]
  (command/run
    args ; it sure would be nice if *command-line-args* worked
    (command/group "enjoy music with more parts of your brain"
                   [["gnuplot" gnuplot/cmd]
                    ["quildemo" quildemo/cmd]
                    ["freqdemo" freqdemo/cmd]
                    ["visualize" visualize/cmd]])))
