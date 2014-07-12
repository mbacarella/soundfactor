(defproject soundfactor "1.0"
  :description "soundfactor: find duplicate and near-duplicate mp3s"
  :main soundfactor.core
  :aot [soundfactor.core]
  :shell-wrapper true
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojars.technomancy/jlayer "1.0"]
                 [org.clojars.automata/tritonus-share "1.0.0"]
                 [org.clojars.automata/mp3spi "1.9.4"]])