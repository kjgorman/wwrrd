(defproject wwrrd "0.1.0-SNAPSHOT"
  :description "what would rick ross do?"
  :url "www.rr.do"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2173"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [om "0.5.0"]]

  :plugins [[lein-cljsbuild "1.0.2"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "wwrrd"
              :source-paths ["app"]
              :compiler {
                :output-to "static/app.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
