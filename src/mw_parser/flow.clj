(ns ^{:doc "A very simple parser which parses flow rules."
      :author "Simon Brooke"}
 mw-parser.flow
  (:require [clojure.string :refer [join]]
            [instaparse.core :as insta]
            [mw-parser.declarative :refer [common-grammar keywords-for-locale]]))

(def flow-grammar
  "Grammar for flow rules"
  (join "\n" ["FLOW-RULE := FLOW SPACE QUANTITY SPACE PROPERTY SPACE FROM SPACE SOURCE SPACE TO-HOW SPACE DESTINATION;"
              "PERCENTAGE := NUMBER #'%';"
              "QUANTITY := PERCENTAGE | NUMBER;"
              "SOURCE := STATE | STATE SPACE WITH SPACE CONDITIONS;"
              "DESTINATION := STATE | STATE SPACE WITH SPACE FLOW-CONDITIONS;"
              "DETERMINER := MOST | LEAST;"
              "DETERMINER-CONDITION := DETERMINER SPACE PROPERTY | DETERMINER SPACE PROPERTY SPACE WITHIN SPACE NUMBER;"
              "FLOW-CONDITIONS := DETERMINER-CONDITION | CONDITIONS"
              "STATE := SYMBOL;"
              "TO-HOW := TO | TO-EACH | TO-FIRST;"
              "TO-EACH := TO SPACE EACH | TO SPACE ALL;"
              "TO-FIRST := TO SPACE EACH"
              ]))

(def parse-flow
  "Parse the argument, assumed to be a string in the correct syntax, and return a parse tree."
  (insta/parser (join "\n" [flow-grammar common-grammar (keywords-for-locale)])))
