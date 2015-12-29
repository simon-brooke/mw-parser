(ns mw-parser.declarative-test
  (:use clojure.pprint
        mw-engine.core
        mw-engine.world)
  (:require [clojure.test :refer :all]
            [mw-parser.declarative :refer :all]))

(deftest rules-tests
  (testing "Rule parser - does not test whether generated functions actually work, just that something is generated!"
    (is (rule? (parse-rule "if state is forest then state should be climax")))
    (is (rule? (parse-rule "if state is in grassland or pasture or heath then state should be village")))
    (is (rule? (parse-rule "if altitude is less than 100 and state is forest then state should be climax and deer should be 3")))
    (is (rule? (parse-rule "if altitude is 100 or fertility is 25 then state should be heath and fertility should be 24.3")))
    (is (rule? (parse-rule "if altitude is 100 or fertility is 25 then state should be heath")))
    (is (rule? (parse-rule "if deer is more than 2 and wolves is 0 and fertility is more than 20 then deer should be deer + 2")))
    (is (rule? (parse-rule "if deer is more than 1 and wolves is more than 1 then deer should be deer - wolves")))
    (is (rule? (parse-rule "if state is forest and fertility is between 55 and 75 then state should be climax")))
    (is (rule? (parse-rule "if fertility is between 55 and 75 then state should be climax")))
    (is (rule? (parse-rule "if altitude is less than 100 and state is forest then state should be climax and deer should be 3")))
    ))

(deftest neighbours-rules-tests
  (testing "Rules which relate to neighbours - hard!"
    (is (rule? (parse-rule "if state is climax and some neighbours have state equal to fire then 3 chance in 5 state should be fire")))
    (is (rule? (parse-rule "if state is in grassland or pasture or heath and 4 neighbours have state equal to water then state should be village")))
    (is (rule? (parse-rule "if 6 neighbours have state equal to water then state should be village")))
    (is (rule? (parse-rule "if state is grassland and 4 neighbours have state equal to water then state should be village")))
    (is (rule? (parse-rule "if state is pasture and more than 3 neighbours have state equal to scrub then state should be scrub")))
    (is (rule? (parse-rule "if state is in grassland or pasture or heath and 4 neighbours have state equal to water then state should be village")))
    (is (rule? (parse-rule "if state is grassland and 4 neighbours have state equal to water then state should be village")))
    (is (rule? (parse-rule "if 6 neighbours have state equal to water then state should be village")))
    ))

(deftest expressions-tests
  (testing "Generating primitive expressions."
    (is (generate '(:NUMERIC-EXPRESSION (:NUMBER "50"))) 50)
    (is (generate '(:NUMERIC-EXPRESSION (:SYMBOL "sealevel")))
        '(:sealevel cell))
    ))

(deftest lhs-generators-tests
  (testing "Generating left-hand-side fragments of rule functions from appropriate fragments of parse trees"
    (is (generate
         '(:PROPERTY-CONDITION (:SYMBOL "state") [:EQUIVALENCE [:IS "is"]] (:SYMBOL "forest")))
        '(= (:state cell) :forest))
    (is (generate
         '(:PROPERTY-CONDITION (:SYMBOL "fertility") [:EQUIVALENCE [:IS "is"]] (:NUMBER "10")))
        '(= (:fertility cell) 10))
    (is (generate '(:PROPERTY-CONDITION (:SYMBOL "fertility") [:COMPARATIVE [:LESS "less"]] (:NUMBER "10")))
        '(< (:fertility cell) 10))
    (is (generate '(:PROPERTY-CONDITION (:SYMBOL "fertility") [:COMPARATIVE [:MORE "more"]] (:NUMBER "10")))
        '(> (:fertility cell) 10))
    (is (generate '(:CONJUNCT-CONDITION (:PROPERTY-CONDITION (:SYMBOL "state") [:EQUIVALENCE [:IS "is"]] (:SYMBOL "forest")) (:AND "and") (:PROPERTY-CONDITION (:SYMBOL "fertility") [:EQUIVALENCE [:IS "is"]] (:NUMBER "10"))))
        '(and (= (:state cell) :forest) (= (:fertility cell) 10)))
    (is (generate '(:DISJUNCT-CONDITION (:PROPERTY-CONDITION (:SYMBOL "state") [:EQUIVALENCE [:IS "is"]] (:SYMBOL "forest")) (:OR "or") (:PROPERTY-CONDITION (:SYMBOL "fertility") [:EQUIVALENCE [:IS "is"]] (:NUMBER "10"))))
        '(or (= (:state cell) :forest) (= (:fertility cell) 10)))
    (is (generate '(:PROPERTY-CONDITION (:SYMBOL "state") [:EQUIVALENCE [:IS "is"]] (:DISJUNCT-EXPRESSION (:IN "in") (:DISJUNCT-VALUE (:SYMBOL "grassland") (:OR "or") (:DISJUNCT-VALUE (:SYMBOL "pasture") (:OR "or") (:DISJUNCT-VALUE (:SYMBOL "heath")))))))
        '(let [value (:state cell)] (some (fn [i] (= i value)) (quote (:grassland :pasture :heath)))))
    (is (generate '(:PROPERTY-CONDITION (:SYMBOL "altitude") [:EQUIVALENCE [:IS "is"]] (:RANGE-EXPRESSION (:BETWEEN "between") (:NUMERIC-EXPRESSION (:NUMBER "50")) (:AND "and") (:NUMERIC-EXPRESSION (:NUMBER "100")))))
        '(let [lower (min 50 100) upper (max 50 100)] (and (>= (:altitude cell) lower) (<= (:altitude cell) upper))))
    ))

(deftest rhs-generators-tests
  (testing "Generating right-hand-side fragments of rule functions from appropriate fragments of parse trees"
    (is (generate
         '(:SIMPLE-ACTION (:SYMBOL "state") (:BECOMES "should be") (:SYMBOL "climax")))
        '(merge cell {:state :climax}))
    (is (generate
         '(:SIMPLE-ACTION (:SYMBOL "fertility") (:BECOMES "should be") (:NUMBER "10")))
         '(merge cell {:fertility 10}))
    ))

(deftest full-generation-tests
  (testing "Full rule generation from pre-parsed tree"
    (is (generate '(:RULE (:IF "if") (:PROPERTY-CONDITION (:SYMBOL "state") [:EQUIVALENCE [:IS "is"]] (:SYMBOL "forest")) (:SIMPLE-ACTION (:SYMBOL "state") (:BECOMES "should be") (:SYMBOL "climax"))))
        '(fn [cell world] (if (= (:state cell) :forest) (merge cell {:state :climax}))))
    ))


(deftest exception-tests
  (testing "Constructions which should cause exceptions to be thrown"
    (is (thrown-with-msg? Exception #"^I did not understand.*"
                          (compile-rule "the quick brown fox jumped over the lazy dog"))
        "Exception thrown if rule text does not match grammar")
    (is (thrown-with-msg? Exception #"^I did not understand.*"
                          (compile-rule "if i have a cat on my lap then everything is fine"))
        "Exception thrown if rule text does not match grammar")
     (is (thrown-with-msg?
          Exception #"The properties 'x' and 'y' of a cell are reserved and should not be set in rule actions"
          (compile-rule "if state is new then x should be 0"))
         "Exception thrown on attempt to set 'x'")
     (is (thrown-with-msg?
          Exception #"The properties 'x' and 'y' of a cell are reserved and should not be set in rule actions"
          (compile-rule "if state is new then y should be 0"))
         "Exception thrown on attempt to set 'y'")
    ))

(deftest compilation-tests
  (testing "Full compilation of rules"

    ))
