(ns ^{:doc "A very simple parser which parses flow rules."
      :author "Simon Brooke"}
 mw-parser.flow
  (:require [clojure.string :refer [join]]
            [mw-parser.declarative :refer [build-parser]]
            [mw-parser.simplify :refer [simplify-second-of-two]]))

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

   `flow 1 food from house having food > 1 to house with least food within 2`
   "
  (join "\n" ["FLOW-RULE := FLOW SPACE QUANTITY SPACE PROPERTY SPACE FROM SPACE SOURCE SPACE TO-HOW SPACE DESTINATION;"
              "PERCENTAGE := NUMBER #'%';"
              "QUANTITY := PERCENTAGE | NUMBER | EXPRESSION | SOME;"
              "SOURCE := STATE | STATE SPACE WITH SPACE CONDITIONS;"
              "DESTINATION := STATE | STATE SPACE WITH SPACE FLOW-CONDITIONS | STATE SPACE WITHIN SPACE VALUE SPACE WITH SPACE FLOW-CONDITIONS;"
              "DETERMINER := MOST | LEAST;"
              "DETERMINER-CONDITION := DETERMINER SPACE PROPERTY | DETERMINER SPACE PROPERTY;"
              "FLOW-CONDITIONS := DETERMINER-CONDITION | CONDITIONS"
              "STATE := SYMBOL;"
              "TO-HOW := TO | TO-EACH | TO-FIRST;"
              "TO-EACH := TO SPACE EACH | TO SPACE ALL;"
              "TO-FIRST := TO SPACE FIRST"]))

(def parse-flow
  "Parse the argument, assumed to be a string in the correct syntax, and return a parse tree."
  (build-parser flow-grammar))

(defn simplify-flow
  [tree]
  (if (coll? tree)
    (case (first tree)
      :CONDITION (simplify-second-of-two tree)
      :CONDITIONS (simplify-second-of-two tree)
      :DETERMINER (simplify-second-of-two tree)
;;      :DETERMINER-CONDITION (simplify-determiner-condition tree)
      :EXPRESSION (simplify-second-of-two tree)
      :FLOW nil
;;      :FLOW-CONDITIONS (simplify-second-of-two tree)
      :PROPERTY (simplify-second-of-two tree)
      :PROPERTY-CONDITION-OR-EXPRESSION (simplify-second-of-two tree)
      :SPACE nil
      :QUANTITY (simplify-second-of-two tree)
      :STATE (list :PROPERTY-CONDITION
                   (list :SYMBOL "state")
                   '(:QUALIFIER
                     (:EQUIVALENCE
                      (:IS "is")))
                   (list :EXPRESSION
                         (list :VALUE (second tree))))
      (remove nil? (map simplify-flow tree)))
    tree))

