(ns mw-parser.declarative-test
  (:require [clojure.string :refer [join]]
            [clojure.test :refer [deftest is testing]]
            [mw-engine.core :refer [transform-world]]
            [mw-engine.utils :refer [get-cell]]
            [mw-engine.world :refer [make-world]]
            [mw-parser.declarative :refer [compile parse]]
            [mw-parser.generate :refer [generate]]
            [mw-parser.simplify :refer [simplify]]
            [mw-parser.utils :refer [suitable-fragment?]]))


(defn rule?
  "Return true if the argument appears to be a parsed rule tree, else false."
  [maybe-rule]
  (suitable-fragment? maybe-rule :RULE))

(deftest rules-tests
  (testing "Rule parser - does not test whether generated functions actually work, just that something is generated!"
    (is (rule? (parse "if state is forest then state should be climax")))
    (is (rule? (parse "if state is in grassland or pasture or heath then state should be village")))
    (is (rule? (parse "if altitude is less than 100 and state is forest then state should be climax and deer should be 3")))
    (is (rule? (parse "if altitude is 100 or fertility is 25 then state should be heath and fertility should be 24.3")))
    (is (rule? (parse "if altitude is 100 or fertility is 25 then state should be heath")))
    (is (rule? (parse "if deer is more than 2 and wolves is 0 and fertility is more than 20 then deer should be deer + 2")))
    (is (rule? (parse "if deer is more than 1 and wolves is more than 1 then deer should be deer - wolves")))
    (is (rule? (parse "if state is forest and fertility is between 55 and 75 then state should be climax")))
    (is (rule? (parse "if fertility is between 55 and 75 then state should be climax")))
    (is (rule? (parse "if altitude is less than 100 and state is forest then state should be climax and deer should be 3")))))

(deftest neighbours-rules-tests
  (testing "Rules which relate to neighbours - hard!"
    (is (rule? (parse "if state is climax and some neighbours have state equal to fire then 3 chance in 5 state should be fire")))
    (is (rule? (parse "if state is in grassland or pasture or heath and 4 neighbours have state equal to water then state should be village")))
    (is (rule? (parse "if 6 neighbours have state equal to water then state should be village")))
    (is (rule? (parse "if state is grassland and 4 neighbours have state equal to water then state should be village")))
    (is (rule? (parse "if state is pasture and more than 3 neighbours have state equal to scrub then state should be scrub")))
    (is (rule? (parse "if state is in grassland or pasture or heath and 4 neighbours have state equal to water then state should be village")))
    (is (rule? (parse "if state is grassland and 4 neighbours have state equal to water then state should be village")))
    (is (rule? (parse "if 6 neighbours have state equal to water then state should be village")))))


(deftest exception-tests
  (testing "Constructions which should cause exceptions to be thrown"
    (is (thrown-with-msg? Exception #"^I did not understand.*"
                          (parse "the quick brown fox jumped over the lazy dog"))
        "Exception thrown if rule text does not match grammar")
    (is (thrown-with-msg? Exception #"^I did not understand.*"
                          (parse "if i have a cat on my lap then everything is fine"))
        "Exception thrown if rule text does not match grammar")
    ;; TODO: these two should be moved to generate-test; the exception should be
    ;; being thrown (but isn't) in the generate phase.
    (is (thrown-with-msg?
         Exception #"The properties 'x' and 'y' of a cell are reserved and should not be set in rule actions"
         (generate (simplify (parse "if state is new then x should be 0"))
        "Exception thrown on attempt to set 'x'")
    (is (thrown-with-msg?
         Exception #"The properties 'x' and 'y' of a cell are reserved and should not be set in rule actions"
         (generate (simplify (parse "if state is new then y should be 0"))))
        "Exception thrown on attempt to set 'y'")))

(deftest correctness-tests
  ;; these are, in so far as possible, the same as the correctness-tests in core-tests - i.e., the two compilers
  ;; compile the same language.
  (testing "Simplest possible rule"
    (let [afn (first (compile "if state is new then state should be grassland"))]
      (is (= (apply afn (list {:state :new} nil))
             {:state :grassland})
          "Rule fires when condition is met")
      (is (nil? (apply afn (list {:state :forest} nil)))
          "Rule doesn't fire when condition isn't met")))

  (testing "Condition conjunction rule"
    (let [afn (first (compile "if state is new and altitude is 0 then state should be water"))]
      (is (= (apply afn (list {:state :new :altitude 0} nil))
             {:state :water :altitude 0})
          "Rule fires when conditions are met")
      (is (nil? (apply afn (list {:state :new :altitude 5} nil)))
          "Rule does not fire: second condition not met")
      (is (nil?  (apply afn (list {:state :forest :altitude 0} nil)))
          "Rule does not fire: first condition not met")))

  (testing "Condition disjunction rule"
    (let [afn (first (compile "if state is new or state is waste then state should be grassland"))]
      (is (= (apply afn (list {:state :new} nil))
             {:state :grassland})
          "Rule fires: first condition met")
      (is (= (apply afn (list {:state :waste} nil))
             {:state :grassland})
          "Rule fires: second condition met")
      (is (nil?  (apply afn (list {:state :forest} nil)))
          "Rule does not fire: neither condition met")))

  (testing "Simple negation rule"
    (let [afn (first (compile "if state is not new then state should be grassland"))]
      (is (nil? (apply afn (list {:state :new} nil)))
          "Rule doesn't fire when condition isn't met")
      (is (= (apply afn (list {:state :forest} nil))
             {:state :grassland})
          "Rule fires when condition is met")))

  (testing "Can't set x or y properties"
    (is (thrown-with-msg?
         Exception #"The properties 'x' and 'y' of a cell are reserved and should not be set in rule actions"
         (first (compile "if state is new then x should be 0")))
        "Exception thrown on attempt to set 'x'")
    (is (thrown-with-msg?
         Exception #"The properties 'x' and 'y' of a cell are reserved and should not be set in rule actions"
         (first (compile "if state is new then y should be 0")))
        "Exception thrown on attempt to set 'y'"))

  (testing "Simple list membership rule"
    (let [afn (first (compile "if state is in heath or scrub or forest then state should be climax"))]
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
    (let [afn (first (compile "if state is not in heath or scrub or forest then state should be climax"))]
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
    (let [afn (first (compile "if altitude is more than 200 then state should be snow"))]
      (is (= (apply afn (list {:altitude 201} nil))
             {:state :snow :altitude 201})
          "Rule fires when condition is met")
      (is (nil? (apply afn (list {:altitude 200} nil)))
          "Rule does not fire when condition is not met")))

  (testing "Property is more than property"
    (let [afn (first (compile "if wolves are more than deer then deer should be 0"))]
      (is (= (apply afn (list {:deer 2 :wolves 3} nil))
             {:deer 0 :wolves 3})
          "Rule fires when condition is met")
      (is (nil? (apply afn (list {:deer 3 :wolves 2} nil)))
          "Rule does not fire when condition is not met")))

  (testing "Property is less than numeric-value"
    (let [afn (first (compile "if altitude is less than 10 then state should be water"))]
      (is (= (apply afn (list {:altitude 9} nil))
             {:state :water :altitude 9})
          "Rule fires when condition is met")
      (is (nil? (apply afn (list {:altitude 10} nil)))
          "Rule does not fire when condition is not met")))

  (testing "Property is less than property"
    (let [afn (first (compile "if wolves are less than deer then deer should be deer - wolves"))]
      (is (= (apply afn (list {:deer 3 :wolves 2} nil))
             {:deer 1 :wolves 2})
          "Rule fires when condition is met")
      (is (nil? (apply afn (list {:deer 2 :wolves 3} nil)))
          "Rule does not fire when condition is not met")))

  (testing "Number neighbours have property equal to value"
    (let [afn (first (compile "if 3 neighbours have state equal to new then state should be water"))
          world (make-world 3 3)]
      (is (= (apply afn (list {:x 0 :y 0} world))
             {:state :water :x 0 :y 0})
          "Rule fires when condition is met (in a new world all cells are new, corner cell has three neighbours)")
      (is (nil? (apply afn (list {:x 1 :y 1} world)))
          "Middle cell has eight neighbours, so rule does not fire."))
    (let [afn (first (compile "if 3 neighbours are new then state should be water"))
          world (make-world 3 3)]
      ;; 'are new' and 'is new' should be the same as 'have state equal to new'
      (is (= (apply afn (list {:x 0 :y 0} world))
             {:state :water :x 0 :y 0})
          "Rule fires when condition is met (in a new world all cells are new, corner cell has three neighbours)")
      (is (nil? (apply afn (list {:x 1 :y 1} world)))
          "Middle cell has eight neighbours, so rule does not fire."))
    (let [afn (first (compile "if 3 neighbours is new then state should be water"))
          world (make-world 3 3)]
      ;; 'are new' and 'is new' should be the same as 'have state equal to new'
      (is (= (apply afn (list {:x 0 :y 0} world))
             {:state :water :x 0 :y 0})
          "Rule fires when condition is met (in a new world all cells are new, corner cell has three neighbours)")
      (is (nil? (apply afn (list {:x 1 :y 1} world)))
          "Middle cell has eight neighbours, so rule does not fire.")))

  (testing "Number neighbours have property more than numeric-value"
    ;; if 3 neighbours have altitude more than 10 then state should be beach
    (let [afn (first (compile "if 3 neighbours have altitude more than 10 then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile (join "\n" ["if x is 2 then altitude should be 11" 
                                            "if x is less than 2 then altitude should be 0"])))]
      (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
          "Rule fires when condition is met (strip of altitude 11 down right hand side)")
      (is (nil? (apply afn (list {:x 2 :y 1} world)))
          "Middle cell of the strip has only two high neighbours, so rule should not fire.")))

  (testing "Number neighbours have property less than numeric-value"
    (let [afn (first (compile "if 5 neighbours have altitude less than 10 then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile (join "\n" ["if x is 2 then altitude should be 11"
                                      "if x is less than 2 then altitude should be 0"])))]
      (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
          "Rule fires when condition is met (strip of altitude 11 down right hand side)")
      (is (nil? (apply afn (list {:x 2 :y 1} world)))
          "Middle cell of the strip has two high neighbours, so rule should not fire.")))

  (testing "More than number neighbours have property equal to numeric-value"
    (let [afn (first (compile "if more than 2 neighbours have altitude equal to 11 then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile (join "\n" ["if x is 2 then altitude should be 11" 
                                 "if x is less than 2 then altitude should be 0"])))]
      (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
          "Rule fires when condition is met (strip of altitude 11 down right hand side)")
      (is (nil? (apply afn (list {:x 2 :y 1} world)))
          "Middle cell of the strip has only two high neighbours, so rule should not fire.")))

  (testing "More than number neighbours have property equal to symbolic-value"
    (let [afn (first (compile "if more than 2 neighbours have state equal to grassland then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile 
                  (join "\n" 
                        (list "if x is 2 then altitude should be 11 and state should be grassland"
                                       "if x is less than 2 then altitude should be 0 and state should be water"))))]
      (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
          "Rule fires when condition is met (strip of altitude 11 down right hand side)")
      (is (nil? (apply afn (list {:x 2 :y 1} world)))
          "Middle cell of the strip has only two high neighbours, so rule should not fire."))
    (let [afn (first (compile "if more than 2 neighbours are grassland then state should be beach"))
          ;; 'are grassland' should mean the same as 'have state equal to grassland'.
          world (transform-world
                 (make-world 3 3)
                 (compile (join "\n" (list "if x is 2 then altitude should be 11 and state should be grassland"
                                    "if x is less than 2 then altitude should be 0 and state should be water"))))]
      (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
          "Rule fires when condition is met (strip of altitude 11 down right hand side)")
      (is (nil? (apply afn (list {:x 2 :y 1} world)))
          "Middle cell of the strip has only two high neighbours, so rule should not fire.")))

  (testing "Fewer than number neighbours have property equal to numeric-value"
    (let [afn (first (compile "if fewer than 3 neighbours have altitude equal to 11 then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile (join "\n" (list "if x is 2 then altitude should be 11"
                                           "if x is less than 2 then altitude should be 0"))))]
      (is (= (:state (apply afn (list {:x 2 :y 1} world))) :beach)
          "Rule fires when condition is met (Middle cell of the strip has only two high neighbours)")
      (is (nil? (apply afn (list {:x 1 :y 1} world)))
          "Middle cell of world has three high neighbours, so rule should not fire.")))

  (testing "Fewer than number neighbours have property equal to symbolic-value"
    (let [afn (first (compile "if fewer than 3 neighbours have state equal to grassland then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile 
                  (join "\n" 
                        (list "if x is 2 then altitude should be 11 and state should be grassland"
                                           "if x is less than 2 then altitude should be 0 and state should be water"))))]
      (is (= (:state (apply afn (list {:x 2 :y 1} world))) :beach)
          "Rule fires when condition is met (Middle cell of the strip has only two high neighbours)")
      (is (nil? (apply afn (list {:x 1 :y 1} world)))
          "Middle cell of world has three high neighbours, so rule should not fire.")))

  ;; some neighbours have property equal to value
  (testing "Some neighbours have property equal to numeric-value"
    (let [afn (first (compile "if some neighbours have altitude equal to 11 then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile
                  (join "\n"
                        (list  "if x is 2 then altitude should be 11"
                               "if x is less than 2 then altitude should be 0"))))]
      (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
          "Rule fires when condition is met (strip of altitude 11 down right hand side)")
      (is (nil? (apply afn (list {:x 0 :y 1} world)))
          "Left hand side of world has no high neighbours, so rule should not fire.")))

  (testing "Some neighbours have property equal to symbolic-value"
    (let [afn (first (compile "if some neighbours have state equal to grassland then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile
                  (join "\n"
                        (list "if x is 2 then altitude should be 11 and state should be grassland" 
                              "if x is less than 2 then altitude should be 0 and state should be water"))))]
      (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
          "Rule fires when condition is met (strip of altitude 11 down right hand side)")
      (is (nil? (apply afn (list {:x 0 :y 1} world)))
          "Left hand side of world has no high neighbours, so rule should not fire.")))

  ;; more than number neighbours have property more than numeric-value
  (testing "More than number neighbours have property more than symbolic-value"
    (let [afn (first (compile "if more than 2 neighbours have altitude more than 10 then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile
                  (join "\n"
                        (list "if x is 2 then altitude should be 11 and state should be grassland"
                              "if x is less than 2 then altitude should be 0 and state should be water"))))]
      (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
          "Rule fires when condition is met (strip of altitude 11 down right hand side)")
      (is (nil? (apply afn (list {:x 2 :y 1} world)))
          "Middle cell of the strip has only two high neighbours, so rule should not fire.")))

  ;; fewer than number neighbours have property more than numeric-value
  (testing "Fewer than number neighbours have property more than numeric-value"
    (let [afn (first (compile "if fewer than 3 neighbours have altitude more than 10 then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile
                  (join "\n"
                        (list "if x is 2 then altitude should be 11"
                              "if x is less than 2 then altitude should be 0"))))]
      (is (= (:state (apply afn (list {:x 2 :y 1} world))) :beach)
          "Rule fires when condition is met (Middle cell of the strip has only two high neighbours)")
      (is (nil? (apply afn (list {:x 1 :y 1} world)))
          "Middle cell of world has three high neighbours, so rule should not fire.")))

  ;; some neighbours have property more than numeric-value
  (testing "Some neighbours have property more than numeric-value"
    (let [afn (first (compile "if some neighbours have altitude more than 10 then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile
                  (join "\n"
                        (list "if x is 2 then altitude should be 11"
                              "if x is less than 2 then altitude should be 0"))))]
      (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
          "Rule fires when condition is met (strip of altitude 11 down right hand side)")
      (is (nil? (apply afn (list {:x 0 :y 1} world)))
          "Left hand side of world has no high neighbours, so rule should not fire.")))

  ;; more than number neighbours have property less than numeric-value
  (testing "More than number neighbours have property less than numeric-value"
    (let [afn (first (compile "if more than 4 neighbours have altitude less than 10 then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile
                  (join "\n"
                        (list "if x is 2 then altitude should be 11"
                              "if x is less than 2 then altitude should be 0"))))]
      (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
          "Rule fires when condition is met (strip of altitude 11 down right hand side)")
      (is (nil? (apply afn (list {:x 2 :y 1} world)))
          "Middle cell of the strip has only three low neighbours, so rule should not fire.")))

  ;; fewer than number neighbours have property less than numeric-value
  (testing "Fewer than number neighbours have property less than numeric-value"
    (let [afn (first (compile "if fewer than 4 neighbours have altitude less than 10 then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile
                  (join "\n"
                        (list "if x is 2 then altitude should be 11"
                              "if x is less than 2 then altitude should be 0"))))]
      (is (nil? (apply afn (list {:x 1 :y 1} world)))
          "Centre cell has five low neighbours, so rule should not fire")
      (is (= (:state (apply afn (list {:x 2 :y 1} world))) :beach)
          "Middle cell of the strip has only three low neighbours, so rule should  fire.")))

  ;; some neighbours have property less than numeric-value
  (testing "Some number neighbours have property less than numeric-value"
    (let [afn (first (compile "if some neighbours have altitude less than 10 then state should be beach"))
          world (transform-world
                 (make-world 3 3)
                 (compile
                  (join "\n"
                        (list "if x is less than 2 then altitude should be 11"
                              "if x is 2 then altitude should be 0"))))]
      (is (= (:state (apply afn (list {:x 1 :y 1} world))) :beach)
          "Rule fires when condition is met (strip of altitude 0 down right hand side)")
      (is (nil? (apply afn (list {:x 0 :y 1} world)))
          "Left of world is all high, so rule should not fire.")))


  ;; 'single action' already tested in 'condition' tests above
  ;; action and actions
  (testing "Conjunction of actions"
    (let [afn (first (compile "if state is new then state should be grassland and fertility should be 0"))]
      (is (= (apply afn (list {:state :new} nil))
             {:state :grassland :fertility 0})
          "Both actions are executed")))

  ;; 'property should be symbolic-value' and 'property should be numeric-value'
  ;; already tested in tests above

  ;; number chance in number property should be value
  (testing "Syntax of probability rule - action of real probability very hard to test"
    (let [afn (first (compile "if state is forest then 5 chance in 5 state should be climax"))]
      (is (= (:state (apply afn (list {:state :forest} nil))) :climax)
          "five chance in five should fire every time"))
    (let [afn (first (compile "if state is forest then 0 chance in 5 state should be climax"))]
      (is (nil? (apply afn (list {:state :forest} nil)))
          "zero chance in five should never fire")))

  ;; property operator numeric-value
  (testing "Arithmetic action: addition of number"
    (let [afn (first (compile "if state is climax then fertility should be fertility + 1"))]
      (is (= (:fertility
              (apply afn (list {:state :climax :fertility 0} nil)))
             1)
          "Addition is executed")))

  (testing "Arithmetic action: addition of property value"
    (let [afn (first (compile "if state is climax then fertility should be fertility + leaffall"))]
      (is (= (:fertility
              (apply afn
                     (list {:state :climax
                            :fertility 0
                            :leaffall 1} nil)))
             1)
          "Addition is executed")))

  (testing "Arithmetic action: subtraction of number"
    (let [afn (first (compile "if state is crop then fertility should be fertility - 1"))]
      (is (= (:fertility
              (apply afn (list {:state :crop :fertility 2} nil)))
             1)
          "Action is executed")))

  (testing "Arithmetic action: subtraction of property value"
    (let [afn (first (compile "if wolves are more than 0 then deer should be deer - wolves"))]
      (is (= (:deer
              (apply afn
                     (list {:deer 3
                            :wolves 2} nil)))
             1)
          "Action is executed")))

  (testing "Arithmetic action: multiplication by number"
    (let [afn (first (compile "if deer are more than 1 then deer should be deer * 2"))]
      (is (= (:deer
              (apply afn (list {:deer 2} nil)))
             4)
          "Action is executed")))

  (testing "Arithmetic action: multiplication by property value"
    (let [afn (first (compile "if state is crop then deer should be deer * deer"))]
      (is (= (:deer
              (apply afn
                     (list {:state :crop :deer 2} nil)))
             4)
          "Action is executed")))

  (testing "Arithmetic action: division by number"
    (let [afn (first (compile "if wolves are more than 0 then deer should be deer / 2"))]
      (is (= (:deer
              (apply afn (list {:deer 2 :wolves 1} nil)))
             1)
          "Action is executed")))

  (testing "Arithmetic action: division by property value"
    (let [afn (first (compile "if wolves are more than 0 then deer should be deer / wolves"))]
      (is (= (:deer
              (apply afn
                     (list {:deer 2 :wolves 2} nil)))
             1)
          "Action is executed")))

  ;; simple within distance
  (testing "Number neighbours within distance have property equal to value"
    (let [afn (first (compile "if 8 neighbours within 2 have state equal to new then state should be water"))
          world (make-world 5 5)]
      (is (= (apply afn (list {:x 0 :y 0} world))
             {:state :water :x 0 :y 0})
          "Rule fires when condition is met (in a new world all cells are new, corner cell has eight neighbours within two)")
      (is (nil? (apply afn (list {:x 1 :y 1} world)))
          "Middle cell has twenty-four neighbours within two, so rule does not fire.")))

  ;; comparator within distance
  (testing "More than number neighbours within distance have property equal to symbolic-value"
    (let [afn (first (compile "if more than 7 neighbours within 2 have state equal to grassland and more than 7 neighbours within 2 have state equal to water then state should be beach"))
          ;; 5x5 world, strip of high ground two cells wide down left hand side
          ;; xxooo
          ;; xxooo
          ;; xxooo
          ;; xxooo
          ;; xxooo
          world (transform-world
                 (make-world 5 5)
                 (compile
                  (join "\n"
                        (list "if x is less than 2 then altitude should be 11 and state should be grassland"
                              "if x is more than 1 then altitude should be 0 and state should be water"))))]
      (is (= (:state (apply afn (list {:x 2 :y 2} world))) :beach)
          "Rule fires when condition is met (strip of altitude 11 down right hand side)")
      (is (nil? (apply afn (list {:x 0 :y 1} world)))
          "Middle cell of the strip has only two high neighbours, so rule should not fire."))))

(deftest regression-tests
  (testing "Rule in default set which failed on switchover to declarative rules"
    (let [afn (first (compile "if state is scrub then 1 chance in 1 state should be forest"))
          world (transform-world
                 (make-world 3 3)
                 (compile
                  (join "\n"
                        (list "if x is 2 then altitude should be 11"
                              "if x is less than 2 then state should be scrub"))))]
      (is (= (:state (apply afn (list (get-cell world 1 1) world))) :forest)
          "Centre cell is scrub, so rule should fire")
      (is (= (apply afn (list (get-cell world 2 1) world)) nil)
          "Middle cell of the strip is not scrub, so rule should not fire."))))
