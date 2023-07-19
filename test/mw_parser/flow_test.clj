(ns mw-parser.flow-test
  (:require ;; [clojure.pprint :as pprint]
   [clojure.test :refer [deftest is testing]] ;; [mw-engine.core :refer [transform-world]]
   [mw-parser.declarative :refer [parse]]
   [mw-parser.simplify :refer [simplify]]))

(deftest parse-tests
  (testing "flow-grammar"
    (let [rule "flow 1 food from house having food more than 10 to house within 2 with least food"
          expected '(:FLOW-RULE 
                     (:FLOW "flow") 
                     (:QUANTITY (:SIMPLE-EXPRESSION (:NUMBER "1"))) 
                     (:SYMBOL "food") 
                     (:FROM "from") 
                     (:SOURCE 
                      (:PROPERTY-CONDITION (:SYMBOL "state") (:QUALIFIER (:EQUIVALENCE (:IS "is"))) (:EXPRESSION (:VALUE [:SYMBOL "house"]))) 
                      (:WITH "having") 
                      (:PROPERTY-CONDITION (:SYMBOL "food") (:QUALIFIER (:COMPARATIVE-QUALIFIER (:MORE "more") (:THAN "than"))) (:NUMBER "10")))
                     (:TO-HOW (:TO "to")) 
                     (:DESTINATION 
                      (:TARGET 
                       (:PROPERTY-CONDITION (:SYMBOL "state") (:QUALIFIER (:EQUIVALENCE (:IS "is"))) (:EXPRESSION (:VALUE [:SYMBOL "house"]))) 
                       (:RANGE (:WITHIN "within") (:NUMBER "2"))) 
                      (:WITH "with") 
                      (:DETERMINER-CONDITION (:DETERMINER (:LEAST "least")) (:SYMBOL "food")))) 
        
          actual (simplify (parse rule))]

      (is (= actual expected) rule))
    (let [rule "flow 10% food from house having food more than 10 to each house within 2 with food less than 4"
          expected '(:FLOW-RULE
                     (:FLOW "flow")
                     (:QUANTITY (:PERCENTAGE (:NUMBER "10") "%"))
                     (:SYMBOL "food")
                     (:FROM "from")
                     (:SOURCE (:PROPERTY-CONDITION (:SYMBOL "state") (:QUALIFIER (:EQUIVALENCE (:IS "is"))) (:EXPRESSION (:VALUE [:SYMBOL "house"]))) (:WITH "having") (:PROPERTY-CONDITION (:SYMBOL "food") (:QUALIFIER (:COMPARATIVE-QUALIFIER (:MORE "more") (:THAN "than"))) (:NUMBER "10")))
                     (:TO-HOW (:TO-EACH (:TO "to") (:EACH "each")))
                     (:DESTINATION
                      (:TARGET (:PROPERTY-CONDITION (:SYMBOL "state") (:QUALIFIER (:EQUIVALENCE (:IS "is"))) (:EXPRESSION (:VALUE [:SYMBOL "house"])))
                               (:RANGE (:WITHIN "within") (:NUMBER "2")))
                      (:WITH "with") (:PROPERTY-CONDITION (:SYMBOL "food") (:QUALIFIER (:COMPARATIVE-QUALIFIER (:LESS "less") (:THAN "than"))) (:NUMBER "4"))))
          actual (simplify (parse rule))]
      (is (= actual expected) rule))))
