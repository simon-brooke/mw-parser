(defproject mw-parser "unset"
  :description "Parser for production rules for MicroWorld engine"
  :url "http://www.journeyman.cc/microworld"
  :manifest {
             ;; do not reformat this! It is important for the buildall script
             ;; that each of these properties is on a line with nothing else.
		"build-signature-version" "unset"
		"build-signature-user" "Simon Brooke"
		"build-signature-email" "simon@journeyman.cc"
		"build-signature-timestamp" "2014-07-27 11:22:13+01:00"
             }
  :license {:name "GNU General Public License v2"
            :url "http://www.gnu.org/licenses/gpl-2.0.html"}
  :plugins [[lein-marginalia "0.7.1"]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.trace "0.7.8"]
                 [mw-engine "unset"]
                 ])
