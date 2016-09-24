(ns microworld.parser.generate-test
  (:use clojure.pprint
        microworld.engine.core
        microworld.engine.world
        microworld.engine.utils
        microworld.parser.utils)
  (:require [clojure.test :refer :all]
            [microworld.parser.generate :refer :all]))


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
