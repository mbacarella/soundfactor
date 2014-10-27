(defproject soundfactor "1.0"
  :description "soundfactor: find duplicate and near-duplicate mp3s"
  :main soundfactor.core
  :aot [soundfactor.core]
  :shell-wrapper true
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]
                 [org.clojars.technomancy/jlayer "1.0"]
                 [org.clojars.automata/tritonus-share "1.0.0"]
                 [org.clojars.automata/mp3spi "1.9.4"]
                 [net.mikera/vectorz-clj "0.17.0"]
                 [net.mikera/vectorz "0.25.0"]
                 [net.mikera/core.matrix "0.16.0"]
                 [quil "2.2.2"]
                 ])
