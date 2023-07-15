(ns ^{:doc "A very simple parser which parses flow rules."
      :author "Simon Brooke"}
 mw-parser.flow
  (:require [clojure.string :refer [join]]))

(def flow-grammar
  "Grammar for flow rules.
            
   My initial conception of this would be that production rules 
   (if-then rules) and flow rules (flow-from-to rules) would be 
   entirely separate, presented to the parser as separate text 
   files, and parsed and compiled by different chains of functions.
            
   This appears not to be necessary. Flow rules are easy to parse
   with the same parser as production rules -- a lot of the grammar 
   is intentionally common -- and the rules are easily discriminated
   at the compilation ('generate') stage.
   
   The basic rule I want to be able to compile at this stage is the 'mutual
   aid' rule:

   `flow 1 food from house to house within 2 with least food`
   "
  (join "\n" ["FLOW-RULE := FLOW SPACE QUANTITY SPACE PROPERTY SPACE FROM SPACE SOURCE SPACE TO-HOW SPACE DESTINATION;"
              "PERCENTAGE := NUMBER #'%';"
              "QUANTITY := PERCENTAGE | NUMBER | EXPRESSION | SOME;"
              "SOURCE := STATE | STATE SPACE WITH SPACE CONDITIONS;"
              "DESTINATION := TARGET | TARGET SPACE WITH SPACE FLOW-CONDITIONS;"
              "DETERMINER := MOST | LEAST;"
              "DETERMINER-CONDITION := DETERMINER SPACE PROPERTY;"
              "FLOW-CONDITIONS := DETERMINER-CONDITION | CONDITIONS"
              "RANGE := WITHIN SPACE VALUE;"
              "STATE := SYMBOL;"
              "TARGET := STATE | STATE SPACE RANGE;"
              "TO-HOW := TO | TO-EACH | TO-FIRST;"
              "TO-EACH := TO SPACE EACH | TO SPACE ALL;"
              "TO-FIRST := TO SPACE FIRST"]))
