(defproject mw-parser "3.0.0-SNAPSHOT"
  :description "Parser for production rules for MicroWorld engine"
  :url "http://www.journeyman.cc/microworld"
  :manifest {
             "build-signature-version" "unset"
             "build-signature-user" "unset"
             "build-signature-email" "unset"
             "build-signature-timestamp" "unset"
             "Implementation-Version" "unset"
             }
  :source-paths ["src/clj" "src/cljc"]
  :license {:name "GNU General Public License v2"
            :url "http://www.gnu.org/licenses/gpl-2.0.html"}
  :plugins [[lein-marginalia "0.7.1"]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.trace "0.7.9"]
                 [com.lucasbradstreet/instaparse-cljs "1.4.1.2"]
                 [mw-engine "3.0.0-SNAPSHOT"]
                 ])
