(defproject clojure-dla "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.typed "0.3.11"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.8.1"]]
                   :plugins [[lein-typed "0.3.5"]]
                   :core.typed {:check [game-of-life.core]}}})
