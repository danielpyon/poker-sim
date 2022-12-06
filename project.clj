(defproject poker-sim "0.1.0-SNAPSHOT"
  :description "poker simulator"
  :url "https://github.com/danielpyon/poker-sim"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [com.taoensso/nippy "3.2.0"]]
  :main ^:skip-aot poker-sim.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
