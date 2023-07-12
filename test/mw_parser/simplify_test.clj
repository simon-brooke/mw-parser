(ns mw-parser.simplify-test
  (:require [clojure.test :refer [deftest is testing]]
            [mw-parser.declarative :refer [parse-rule]]
            [mw-parser.simplify :refer [simplify]]
            [mw-parser.utils :refer [search-tree]]))

((deftest disjunct-condition-test
   (testing "Generation of disjunct conditions has been producing wrong 
                output -- in a way which didn't actually alter the
                correctness of the rule -- since the beginning, and because
                of inadequate and badly written tests, I didn't know it."
     (let [expected '(:DISJUNCT-CONDITION
                      (:PROPERTY-CONDITION
                       (:SYMBOL "state")
                       (:QUALIFIER (:EQUIVALENCE (:IS "is")))
                       (:SYMBOL "forest"))
                      (:PROPERTY-CONDITION
                       (:SYMBOL "fertility")
                       (:QUALIFIER (:EQUIVALENCE (:IS "is")))
                       (:NUMBER "10")))
           actual (simplify [:DISJUNCT-CONDITION
                                  [:CONDITION
                                   [:PROPERTY-CONDITION
                                    [:PROPERTY [:SYMBOL "state"]]
                                    [:SPACE " "]
                                    [:QUALIFIER [:EQUIVALENCE [:IS "is"]]]
                                    [:SPACE " "]
                                    [:EXPRESSION [:VALUE [:SYMBOL "forest"]]]]]
                                  [:SPACE " "]
                                  [:OR "or"]
                                  [:SPACE " "]
                                  [:CONDITIONS
                                   [:CONDITION
                                    [:PROPERTY-CONDITION
                                     [:PROPERTY [:SYMBOL "fertility"]]
                                     [:SPACE " "]
                                     [:QUALIFIER [:EQUIVALENCE [:IS "is"]]]
                                     [:SPACE " "]
                                     [:EXPRESSION [:VALUE [:NUMBER "10"]]]]]]])]
       (is (= actual expected))))))

(deftest conjunct-condition-test
  (testing "Conjunct conditions were failing in more or less the same way"
    (let [expected '(:CONJUNCT-CONDITION
                     (:PROPERTY-CONDITION
                      (:SYMBOL "state")
                      (:QUALIFIER (:EQUIVALENCE (:IS "is")))
                      (:SYMBOL "forest"))
                     (:PROPERTY-CONDITION
                      (:SYMBOL "fertility")
                      (:QUALIFIER (:EQUIVALENCE (:IS "is")))
                      (:NUMBER "10")))
          actual (simplify [:CONJUNCT-CONDITION
                                 [:CONDITION
                                  [:PROPERTY-CONDITION
                                   [:PROPERTY [:SYMBOL "state"]]
                                   [:SPACE " "]
                                   [:QUALIFIER [:EQUIVALENCE [:IS "is"]]]
                                   [:SPACE " "]
                                   [:EXPRESSION [:VALUE [:SYMBOL "forest"]]]]]
                                 [:SPACE " "]
                                 [:AND "and"]
                                 [:SPACE " "]
                                 [:CONDITIONS
                                  [:CONDITION
                                   [:PROPERTY-CONDITION
                                    [:PROPERTY [:SYMBOL "fertility"]]
                                    [:SPACE " "]
                                    [:QUALIFIER [:EQUIVALENCE [:IS "is"]]]
                                    [:SPACE " "]
                                    [:EXPRESSION [:VALUE [:NUMBER "10"]]]]]]])]
      (is (= actual expected)))))

((deftest unchained-disjuncts-test
   (testing "Disjunct values should not be chained"
     (let [wrong '(:DISJUNCT-EXPRESSION
                   (:IN "in")
                   (:DISJUNCT-VALUE
                    (:SYMBOL "heath")
                    (:DISJUNCT-VALUE
                     (:SYMBOL "scrub")
                     (:DISJUNCT-VALUE (:SYMBOL "forest")))))
           parse-tree (search-tree
                       (parse-rule
                        "if state is not in heath or scrub or forest then state should be climax")
                       :DISJUNCT-EXPRESSION)
           actual (simplify parse-tree)]
       (is (not (= wrong actual))))
     (let [expected '(:DISJUNCT-EXPRESSION
                      (:SYMBOL "heath")
                      (:SYMBOL "scrub")
                      (:SYMBOL "forest"))
           parse-tree (search-tree
                       (parse-rule
                        "if state is not in heath or scrub or forest then state should be climax")
                       :DISJUNCT-EXPRESSION)
           actual (simplify parse-tree)]
       (is (= expected actual))))))