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

(deftest correctness-tests
  ;; these are, in so far as possible, the same as the correctness-tests in core-tests - i.e., the two compilers
  ;; compile the same language.
  (testing "Simplest possible rule"
           (let [afn (compile-rule "if state is new then state should be grassland")]
                 (is (= (apply afn (list {:state :new} nil))
                            {:state :grassland})
                     "Rule fires when condition is met")
                 (is (nil? (apply afn (list {:state :forest} nil))))
                     "Rule doesn't fire when condition isn't met"))

  (testing "Condition conjunction rule"
           (let [afn (compile-rule "if state is new and altitude is 0 then state should be water")]
                 (is (= (apply afn (list {:state :new :altitude 0} nil))
                            {:state :water :altitude 0})
                     "Rule fires when conditions are met")
                 (is (nil? (apply afn (list {:state :new :altitude 5} nil)))
                     "Rule does not fire: second condition not met")
                 (is (nil?  (apply afn (list {:state :forest :altitude 0} nil)))
                     "Rule does not fire: first condition not met")))

  (testing "Condition disjunction rule"
           (let [afn (compile-rule "if state is new or state is waste then state should be grassland")]
                 (is (= (apply afn (list {:state :new} nil))
                            {:state :grassland})
                     "Rule fires: first condition met")
                 (is (= (apply afn (list {:state :waste} nil))
                            {:state :grassland})
                     "Rule fires: second condition met")
                 (is (nil?  (apply afn (list {:state :forest} nil)))
                     "Rule does not fire: neither condition met")))

  (testing "Simple negation rule"
           (let [afn (compile-rule "if state is not new then state should be grassland")]
                 (is (nil? (apply afn (list {:state :new} nil)))
                     "Rule doesn't fire when condition isn't met")
                 (is (= (apply afn (list {:state :forest} nil))
                            {:state :grassland})
                     "Rule fires when condition is met")))

  (testing "Can't set x or y properties"
         (is (thrown-with-msg?
          Exception #"The properties 'x' and 'y' of a cell are reserved and should not be set in rule actions"
          (compile-rule "if state is new then x should be 0"))
         "Exception thrown on attempt to set 'x'")
     (is (thrown-with-msg?
          Exception #"The properties 'x' and 'y' of a cell are reserved and should not be set in rule actions"
          (compile-rule "if state is new then y should be 0"))
         "Exception thrown on attempt to set 'y'"))

  (testing "Simple list membership rule"
           (let [afn (compile-rule "if state is in heath or scrub or forest then state should be climax")]
             (is (= (apply afn (list {:state :heath} nil))
                    {:state :climax})
                 "Rule fires when condition is met")
             (is (= (apply afn (list {:state :scrub} nil))
                    {:state :climax})
                 "Rule fires when condition is met")
             (is (= (apply afn (list {:state :forest} nil))
                    {:state :climax})
                 "Rule fires when condition is met")
             (is (nil? (apply afn (list {:state :grassland} nil)))
                 "Rule does not fire when condition is not met")))

  (testing "Negated list membership rule"
           (let [afn (compile-rule "if state is not in heath or scrub or forest then state should be climax")]
             (is (nil? (apply afn (list {:state :heath} nil)))
                 "Rule does not fire when condition is not met")
             (is (nil? (apply afn (list {:state :scrub} nil)))
                 "Rule does not fire when condition is not met")
             (is (nil? (apply afn (list {:state :forest} nil)))
                 "Rule does not fire when condition is not met")
             (is (= (apply afn (list {:state :grassland} nil))
                    {:state :climax})
                 "Rule fires when condition is met")))

  (testing "Property is more than numeric-value"
           (let [afn (compile-rule "if altitude is more than 200 then state should be snow")]
             (is (= (apply afn (list {:altitude 201} nil))
                    {:state :snow :altitude 201})
                 "Rule fires when condition is met")
             (is (nil? (apply afn (list {:altitude 200} nil)))
                 "Rule does not fire when condition is not met")))

  (testing "Property is more than property"
           (let [afn (compile-rule "if wolves are more than deer then deer should be 0")]
             (is (= (apply afn (list {:deer 2 :wolves 3} nil))
                    {:deer 0 :wolves 3})
                 "Rule fires when condition is met")
             (is (nil? (apply afn (list {:deer 3 :wolves 2} nil)))
                 "Rule does not fire when condition is not met")))

  (testing "Property is less than numeric-value"
           (let [afn (compile-rule "if altitude is less than 10 then state should be water")]
             (is (= (apply afn (list {:altitude 9} nil))
                    {:state :water :altitude 9})
                 "Rule fires when condition is met")
             (is (nil? (apply afn (list {:altitude 10} nil)))
                 "Rule does not fire when condition is not met")))

  (testing "Property is less than property"
           (let [afn (compile-rule "if wolves are less than deer then deer should be deer - wolves")]
             (is (= (apply afn (list {:deer 3 :wolves 2} nil))
                    {:deer 1 :wolves 2})
                 "Rule fires when condition is met")
             (is (nil? (apply afn (list {:deer 2 :wolves 3} nil)))
                 "Rule does not fire when condition is not met")))

  (testing "Number neighbours have property equal to value"
           (let [afn (compile-rule "if 3 neighbours have state equal to new then state should be water")
                 world (make-world 3 3)]
             (is (= (apply afn (list {:x 0 :y 0} world))
                    {:state :water :x 0 :y 0})
                 "Rule fires when condition is met (in a new world all cells are new, corner cell has three neighbours)")
             (is (nil? (apply afn (list {:x 1 :y 1} world)))
                 "Middle cell has eight neighbours, so rule does not fire."))
           (let [afn (compile-rule "if 3 neighbours are new then state should be water")
                 world (make-world 3 3)]
             ;; 'are new' should be the same as 'have state equal to new'
             (is (= (apply afn (list {:x 0 :y 0} world))
                    {:state :water :x 0 :y 0})
                 "Rule fires when condition is met (in a new world all cells are new, corner cell has three neighbours)")
             (is (nil? (apply afn (list {:x 1 :y 1} world)))
                 "Middle cell has eight neighbours, so rule does not fire.")))

  (testing "Number neighbours have property more than numeric-value"
           (let [afn (compile-rule "if 3 neighbours have altitude more than 10 then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11")
                               (compile-rule "if x is less than 2 then altitude should be 0")))]
             (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
                 "Rule fires when condition is met (strip of altitude 11 down right hand side)")
             (is (nil? (apply afn (list {:x 2 :y 1} world)))
                 "Middle cell of the strip has only two high neighbours, so rule should not fire.")))

  (testing "Number neighbours have property less than numeric-value"
           (let [afn (compile-rule "if 5 neighbours have altitude less than 10 then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11")
                               (compile-rule "if x is less than 2 then altitude should be 0")))]
             (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
                 "Rule fires when condition is met (strip of altitude 11 down right hand side)")
             (is (nil? (apply afn (list {:x 2 :y 1} world)))
                 "Middle cell of the strip has two high neighbours, so rule should not fire.")))

  (testing "More than number neighbours have property equal to numeric-value"
           (let [afn (compile-rule "if more than 2 neighbours have altitude equal to 11 then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11")
                               (compile-rule "if x is less than 2 then altitude should be 0")))]
             (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
                 "Rule fires when condition is met (strip of altitude 11 down right hand side)")
             (is (nil? (apply afn (list {:x 2 :y 1} world)))
                 "Middle cell of the strip has only two high neighbours, so rule should not fire.")))

  (testing "More than number neighbours have property equal to symbolic-value"
           (let [afn (compile-rule "if more than 2 neighbours have state equal to grassland then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11 and state should be grassland")
                               (compile-rule "if x is less than 2 then altitude should be 0 and state should be water")))]
             (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
                 "Rule fires when condition is met (strip of altitude 11 down right hand side)")
             (is (nil? (apply afn (list {:x 2 :y 1} world)))
                 "Middle cell of the strip has only two high neighbours, so rule should not fire."))
           (let [afn (compile-rule "if more than 2 neighbours are grassland then state should be beach")
                 ;; 'are grassland' should mean the same as 'have state equal to grassland'.
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11 and state should be grassland")
                               (compile-rule "if x is less than 2 then altitude should be 0 and state should be water")))]
             (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
                 "Rule fires when condition is met (strip of altitude 11 down right hand side)")
             (is (nil? (apply afn (list {:x 2 :y 1} world)))
                 "Middle cell of the strip has only two high neighbours, so rule should not fire."))
           )

  (testing "Fewer than number neighbours have property equal to numeric-value"
           (let [afn (compile-rule "if fewer than 3 neighbours have altitude equal to 11 then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11")
                               (compile-rule "if x is less than 2 then altitude should be 0")))]
             (is (= (:state (apply afn (list {:x 2 :y 1} world))) :beach)
                 "Rule fires when condition is met (Middle cell of the strip has only two high neighbours)")
             (is (nil? (apply afn (list {:x 1 :y 1} world)))
                 "Middle cell of world has three high neighbours, so rule should not fire.")))

  (testing "Fewer than number neighbours have property equal to symbolic-value"
           (let [afn (compile-rule "if fewer than 3 neighbours have state equal to grassland then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11 and state should be grassland")
                               (compile-rule "if x is less than 2 then altitude should be 0 and state should be water")))]
             (is (= (:state (apply afn (list {:x 2 :y 1} world))) :beach)
                 "Rule fires when condition is met (Middle cell of the strip has only two high neighbours)")
             (is (nil? (apply afn (list {:x 1 :y 1} world)))
                 "Middle cell of world has three high neighbours, so rule should not fire.")))

;; some neighbours have property equal to value
  (testing "Some neighbours have property equal to numeric-value"
           (let [afn (compile-rule "if some neighbours have altitude equal to 11 then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11")
                               (compile-rule "if x is less than 2 then altitude should be 0")))]
             (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
                 "Rule fires when condition is met (strip of altitude 11 down right hand side)")
             (is (nil? (apply afn (list {:x 0 :y 1} world)))
                 "Left hand side of world has no high neighbours, so rule should not fire.")))

  (testing "Some neighbours have property equal to symbolic-value"
           (let [afn (compile-rule "if some neighbours have state equal to grassland then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11 and state should be grassland")
                               (compile-rule "if x is less than 2 then altitude should be 0 and state should be water")))]
             (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
                 "Rule fires when condition is met (strip of altitude 11 down right hand side)")
             (is (nil? (apply afn (list {:x 0 :y 1} world)))
                 "Left hand side of world has no high neighbours, so rule should not fire.")))

;; more than number neighbours have property more than numeric-value
  (testing "More than number neighbours have property more than symbolic-value"
           (let [afn (compile-rule "if more than 2 neighbours have altitude more than 10 then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11 and state should be grassland")
                               (compile-rule "if x is less than 2 then altitude should be 0 and state should be water")))]
             (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
                 "Rule fires when condition is met (strip of altitude 11 down right hand side)")
             (is (nil? (apply afn (list {:x 2 :y 1} world)))
                 "Middle cell of the strip has only two high neighbours, so rule should not fire.")))

;; fewer than number neighbours have property more than numeric-value
  (testing "Fewer than number neighbours have property more than numeric-value"
           (let [afn (compile-rule "if fewer than 3 neighbours have altitude more than 10 then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11")
                               (compile-rule "if x is less than 2 then altitude should be 0")))]
             (is (= (:state (apply afn (list {:x 2 :y 1} world))) :beach)
                 "Rule fires when condition is met (Middle cell of the strip has only two high neighbours)")
             (is (nil? (apply afn (list {:x 1 :y 1} world)))
                 "Middle cell of world has three high neighbours, so rule should not fire.")))

;; some neighbours have property more than numeric-value
  (testing "Some neighbours have property more than numeric-value"
           (let [afn (compile-rule "if some neighbours have altitude more than 10 then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11")
                               (compile-rule "if x is less than 2 then altitude should be 0")))]
             (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
                 "Rule fires when condition is met (strip of altitude 11 down right hand side)")
             (is (nil? (apply afn (list {:x 0 :y 1} world)))
                 "Left hand side of world has no high neighbours, so rule should not fire.")))

;; more than number neighbours have property less than numeric-value
  (testing "More than number neighbours have property less than numeric-value"
           (let [afn (compile-rule "if more than 4 neighbours have altitude less than 10 then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11")
                               (compile-rule "if x is less than 2 then altitude should be 0")))]
             (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
                 "Rule fires when condition is met (strip of altitude 11 down right hand side)")
             (is (nil? (apply afn (list {:x 2 :y 1} world)))
                 "Middle cell of the strip has only three low neighbours, so rule should not fire.")))

;; fewer than number neighbours have property less than numeric-value
  (testing "Fewer than number neighbours have property less than numeric-value"
           (let [afn (compile-rule "if fewer than 4 neighbours have altitude less than 10 then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is 2 then altitude should be 11")
                               (compile-rule "if x is less than 2 then altitude should be 0")))]
             (is (nil? (apply afn (list {:x 1 :y 1} world)))
                 "Centre cell has five low neighbours, so rule should not fire")
             (is (= (:state (apply afn (list {:x 2 :y 1} world))) :beach)
                 "Middle cell of the strip has only three low neighbours, so rule should  fire.")))

;; some neighbours have property less than numeric-value
  (testing "Some number neighbours have property less than numeric-value"
           (let [afn (compile-rule "if some neighbours have altitude less than 10 then state should be beach")
                 world (transform-world
                         (make-world 3 3)
                         (list (compile-rule "if x is less than 2 then altitude should be 11")
                               (compile-rule "if x is 2 then altitude should be 0")))]
             (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
                 "Rule fires when condition is met (strip of altitude 0 down right hand side)")
             (is (nil? (apply afn (list {:x 0 :y 1} world)))
                 "Left of world is all high, so rule should not fire.")))


;; 'single action' already tested in 'condition' tests above
;; action and actions
  (testing "Conjunction of actions"
           (let [afn (compile-rule "if state is new then state should be grassland and fertility should be 0")]
                 (is (= (apply afn (list {:state :new} nil))
                            {:state :grassland :fertility 0})
                     "Both actions are executed")))

;; 'property should be symbolic-value' and 'property should be numeric-value'
;; already tested in tests above

;; number chance in number property should be value
  (testing "Syntax of probability rule - action of real probability very hard to test"
           (let [afn (compile-rule "if state is forest then 5 chance in 5 state should be climax")]
             (is (= (:state (apply afn (list {:state :forest} nil))) :climax)
                 "five chance in five should fire every time"))
           (let [afn (compile-rule "if state is forest then 0 chance in 5 state should be climax")]
             (is (nil? (apply afn (list {:state :forest} nil)))
                 "zero chance in five should never fire")))

;; property operator numeric-value
  (testing "Arithmetic action: addition of number"
           (let [afn (compile-rule "if state is climax then fertility should be fertility + 1")]
                 (is (= (:fertility
                          (apply afn (list {:state :climax :fertility 0} nil)))
                        1)
                     "Addition is executed")))

  (testing "Arithmetic action: addition of property value"
           (let [afn (compile-rule "if state is climax then fertility should be fertility + leaf-fall")]
                 (is (= (:fertility
                          (apply afn
                                 (list {:state :climax
                                        :fertility 0
                                        :leaf-fall 1} nil)))
                        1)
                     "Addition is executed")))

  (testing "Arithmetic action: subtraction of number"
           (let [afn (compile-rule "if state is crop then fertility should be fertility - 1")]
                 (is (= (:fertility
                          (apply afn (list {:state :crop :fertility 2} nil)))
                        1)
                     "Action is executed")))

  (testing "Arithmetic action: subtraction of property value"
           (let [afn (compile-rule "if wolves are more than 0 then deer should be deer - wolves")]
                 (is (= (:deer
                          (apply afn
                                 (list {:deer 3
                                        :wolves 2} nil)))
                        1)
                     "Action is executed")))

  (testing "Arithmetic action: multiplication by number"
           (let [afn (compile-rule "if deer are more than 1 then deer should be deer * 2")]
                 (is (= (:deer
                          (apply afn (list {:deer 2} nil)))
                        4)
                     "Action is executed")))

  (testing "Arithmetic action: multiplication by property value"
           (let [afn (compile-rule "if state is crop then deer should be deer * deer")]
                 (is (= (:deer
                          (apply afn
                                 (list {:state :crop :deer 2} nil)))
                        4)
                     "Action is executed")))

  (testing "Arithmetic action: division by number"
           (let [afn (compile-rule "if wolves are more than 0 then deer should be deer / 2")]
                 (is (= (:deer
                          (apply afn (list {:deer 2 :wolves 1} nil)))
                        1)
                     "Action is executed")))

  (testing "Arithmetic action: division by property value"
           (let [afn (compile-rule "if wolves are more than 0 then deer should be deer / wolves")]
                 (is (= (:deer
                          (apply afn
                                 (list {:deer 2 :wolves 2} nil)))
                        1)
                     "Action is executed")))

;; simple within distance
  (testing "Number neighbours within distance have property equal to value"
           (let [afn (compile-rule "if 8 neighbours within 2 have state equal to new then state should be water")
                 world (make-world 5 5)]
              (is (= (apply afn (list {:x 0 :y 0} world))
                    {:state :water :x 0 :y 0})
                 "Rule fires when condition is met (in a new world all cells are new, corner cell has eight neighbours within two)")
             (is (nil? (apply afn (list {:x 1 :y 1} world)))
                 "Middle cell has twenty-four neighbours within two, so rule does not fire.")))

;; comparator within distance
  (testing "More than number neighbours within distance have property equal to symbolic-value"
           (let [afn (compile-rule "if more than 7 neighbours within 2 have state equal to grassland and more than 7 neighbours within 2 have state equal to water then state should be beach")
                 ;; 5x5 world, strip of high ground two cells wide down left hand side
                 ;; xxooo
                 ;; xxooo
                 ;; xxooo
                 ;; xxooo
                 ;; xxooo
                 world (transform-world
                         (make-world 5 5)
                         (list (compile-rule "if x is less than 2 then altitude should be 11 and state should be grassland")
                               (compile-rule "if x is more than 1 then altitude should be 0 and state should be water")))]
             (is (= (:state (apply afn (list {:x 2 :y 2} world))) :beach)
                 "Rule fires when condition is met (strip of altitude 11 down right hand side)")
             (is (nil? (apply afn (list {:x 0 :y 1} world)))
                 "Middle cell of the strip has only two high neighbours, so rule should not fire."))
    ))
