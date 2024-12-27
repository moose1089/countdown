(defproject countdown "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.0"]
                 [nrepl "1.2.0"]
                 [org.clojure/tools.trace "0.7.11"]
                 [dev.weavejester/medley "1.8.1"]
                 [org.clojure/math.combinatorics "0.3.0"]]
  :main ^:skip-aot countdown.core
  :target-path "target/%s"
  :aliases {"v3" ["run" "-m" "countdown.core-3"]
            "v2" ["run" "-m" "countdown.core-2"]
            "v1" ["run" "-m" "countdown.core"]}
  :profiles {:uberjar {:aot :all}})
