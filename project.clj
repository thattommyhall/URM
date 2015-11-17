(defproject urm "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :profiles {:dev {:dependencies [[expectations "1.4.41"]]
                   :plugins [[lein-expectations "0.0.7"]
                             [lein-autoexpect "1.4.2"]]}}
  :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]
  :main urm.core)
