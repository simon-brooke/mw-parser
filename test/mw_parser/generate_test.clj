(ns mw-parser.generate-test 
  (:require [clojure.test :refer [deftest is testing]]
            [mw-parser.generate :refer [generate]]))

;; TODO: these tests are badly written and many (all?!?) of them were not 
;; actually firing. rewrite ALL to the pattern:
;;
;; (let [actual ...
;;       expected ...]
;;    (is (= actual expected)))

(deftest expressions-tests
  (testing "Generating primitive expressions."
    (is (= (generate '(:NUMERIC-EXPRESSION (:NUMBER "50"))) 50))
    (is (= (generate '(:NUMERIC-EXPRESSION (:SYMBOL "sealevel")))
        '(:sealevel cell)))))

(deftest lhs-generators-tests
  (testing "Generating left-hand-side fragments of rule functions from appropriate fragments of parse trees"
    (let [expected '(= (:state cell) (or (:forest cell) :forest))
          actual (generate
           '(:PROPERTY-CONDITION 
             (:SYMBOL "state") 
             [:EQUIVALENCE [:IS "is"]] 
             (:SYMBOL "forest")))] 
      (is (= actual expected)))
    (is (= (generate
         '(:PROPERTY-CONDITION (:SYMBOL "fertility") [:EQUIVALENCE [:IS "is"]] (:NUMBER "10")))
        '(= (:fertility cell) 10)))
    (is (= (generate '(:PROPERTY-CONDITION (:SYMBOL "fertility") [:COMPARATIVE [:LESS "less"]] (:NUMBER "10")))
        '(< (:fertility cell) 10)))
    (is (= (generate '(:PROPERTY-CONDITION (:SYMBOL "fertility") [:COMPARATIVE [:MORE "more"]] (:NUMBER "10")))
        '(> (:fertility cell) 10)))
    (is (= (generate '(:CONJUNCT-CONDITION
                       (:PROPERTY-CONDITION
                        (:SYMBOL "state")
                        (:QUALIFIER (:EQUIVALENCE (:IS "is")))
                        (:SYMBOL "forest"))
                       (:PROPERTY-CONDITION
                        (:SYMBOL "fertility")
                        (:QUALIFIER (:EQUIVALENCE (:IS "is")))
                        (:NUMBER "10"))))
        '(and (= (:state cell) (or (:forest cell) :forest)) (= (:fertility cell) 10))))
    (is (= (generate '(:DISJUNCT-CONDITION (:PROPERTY-CONDITION (:SYMBOL "state") (:EQUIVALENCE (:IS "is")) (:SYMBOL "forest")) (:PROPERTY-CONDITION (:SYMBOL "fertility") (:EQUIVALENCE (:IS "is")) (:NUMBER "10"))))
        '(or (= (:state cell) (or (:forest cell) :forest)) (= (:fertility cell) 10))))
    (is (= (generate '(:PROPERTY-CONDITION
                       (:SYMBOL "state")
                       (:QUALIFIER (:EQUIVALENCE (:IS "is")))
                       (:DISJUNCT-EXPRESSION
                        (:SYMBOL "heath")
                        (:SYMBOL "scrub")
                        (:SYMBOL "forest"))))
        '(#{:scrub :forest :heath} (:state cell))))
    (is (= (generate '(:PROPERTY-CONDITION (:SYMBOL "altitude") [:EQUIVALENCE [:IS "is"]] (:RANGE-EXPRESSION (:BETWEEN "between") (:NUMERIC-EXPRESSION (:NUMBER "50")) (:AND "and") (:NUMERIC-EXPRESSION (:NUMBER "100")))))
        '(let [lower (min 50 100) upper (max 50 100)] (and (>= (:altitude cell) lower) (<= (:altitude cell) upper)))))))

(deftest rhs-generators-tests
  (testing "Generating right-hand-side fragments of rule functions from appropriate fragments of parse trees"
    (is (= (generate
         '(:SIMPLE-ACTION (:SYMBOL "state") (:BECOMES "should be") (:SYMBOL "climax")))
        '(merge cell {:state :climax})))
    (is (= (generate
         '(:SIMPLE-ACTION (:SYMBOL "fertility") (:BECOMES "should be") (:NUMBER "10")))
         '(merge cell {:fertility 10})))))

(deftest full-generation-tests
  (testing "Full rule generation from pre-parsed tree"
    (let [rule '(:RULE
                 (:IF "if")
                 (:PROPERTY-CONDITION
                  (:SYMBOL "state")
                  (:QUALIFIER (:EQUIVALENCE (:IS "is")))
                  (:SYMBOL "forest"))
                 (:ACTIONS
                  (:SIMPLE-ACTION
                   (:SYMBOL "state")
                   (:BECOMES "should be")
                   (:SYMBOL "climax"))))
          expected '(fn [cell world]
                      (when 
                       (= (:state cell) (or (:forest cell) :forest)) 
                        (merge cell {:state :climax})))
          actual (generate rule)
          expected-meta {:rule-type :production}
          actual-meta (meta actual)]
      (is (= actual expected))
      (is (= actual-meta expected-meta)))))
