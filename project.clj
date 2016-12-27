(defproject mw-parser "0.1.6-SNAPSHOT"
  :description "Parser for production rules for MicroWorld engine"
  :url "http://www.journeyman.cc/microworld"
  :manifest {
             "build-signature-version" "unset"
             "build-signature-user" "unset"
             "build-signature-email" "unset"
             "build-signature-timestamp" "unset"
             "Implementation-Version" "unset"
             }
  :license {:name "GNU General Public License v2"
            :url "http://www.gnu.org/licenses/gpl-2.0.html"}
  :plugins [[lein-marginalia "0.7.1"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.trace "0.7.9"]
                 [instaparse "1.4.1"]
                 [mw-engine "0.1.6-SNAPSHOT"]
                 ])
