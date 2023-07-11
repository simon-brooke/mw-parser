(defproject mw-parser "0.2.0-SNAPSHOT"
  :cloverage {:output "docs/cloverage"}
  :codox {:metadata {:doc "**TODO**: write docs"
                     :doc/format :markdown}
          :output-path "docs/codox"
          :source-uri "https://github.com/simon-brooke/mw-parser/blob/master/{filepath}#L{line}"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.trace "0.7.11"]
                 [instaparse "1.4.12"]
                 [mw-engine "0.2.0-SNAPSHOT"]
                 [trptr/java-wrapper "0.2.3"]]
  :description "Parser for production rules for MicroWorld engine"
  :license {:name "GNU General Public License v2"
            :url "http://www.gnu.org/licenses/gpl-2.0.html"}
  :manifest {"build-signature-version" "unset"
             "build-signature-user" "unset"
             "build-signature-email" "unset"
             "build-signature-timestamp" "unset"
             "Implementation-Version" "unset"}
  :plugins [[lein-marginalia "0.7.1"]
            [lein-cloverage "1.2.2"]
            [lein-codox "0.10.8"]]
  :url "http://www.journeyman.cc/microworld")
