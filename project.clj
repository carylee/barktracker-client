(defproject barktracker-client "0.1.0-SNAPSHOT"
  :description "The listening client of an app to moniter a dog's barking"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [incanter/incanter-core "1.5.4"]
                 [incanter/incanter-charts "1.5.4"]
                 [clj-http "0.9.1"]]
  :dev-dependencies [[lein-reload "1.0.0"]]
  :main barktracker-client.core)
