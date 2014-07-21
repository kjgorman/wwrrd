(defproject wwrrd "0.1.0-SNAPSHOT"
  :description "what would rick ross do?"
  :url "www.rr.do"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [org.clojure/core.async "0.1.301.0-deb34a-alpha"]
                 [om "0.5.0"]
                 [cljs-http "0.1.14"]]

  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "wwrrd"
              :source-paths ["app"]
              :compiler {
                :output-to "static/app.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
