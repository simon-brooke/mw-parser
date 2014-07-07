(ns mw-parser.core-test
  (:use mw-engine.utils)
  (:require [clojure.test :refer :all]
            [mw-parser.core :refer :all]))


(deftest rules-tests
  (testing "Rule parser - does not test whether generated functions actually work, just that something is generated!"
           (is (parse-rule "if altitude is less than 100 and state is forest then state should be climax and deer should be 3"))
           (is (parse-rule "if altitude is less than 100 and state is forest then state should be climax and deer should be 3"))
           (is (parse-rule "if altitude is 100 or fertility is 25 then state should be heath and fertility should be 24.3"))
           (is (parse-rule "if altitude is 100 or fertility is 25 then state should be heath"))
           (is (parse-rule "if deer is more than 2 and wolves is 0 and fertility is more than 20 then deer should be deer + 2"))
           (is (parse-rule "if deer is more than 1 and wolves is more than 1 then deer should be deer - wolves"))
           (is (parse-rule "if state is grassland and 4 neighbours have state equal to water then state should be village"))
           (is (parse-rule "if state is forest and fertility is between 55 and 75 then state should be climax"))
           (is (parse-rule "if 6 neighbours have state equal to water then state should be village"))
           (is (parse-rule "if state is in grassland or pasture or heath and 4 neighbours are water then state should be village"))

           ;; ideally should also test that the rule works, but I haven't worked out how to make mw-engine.utils available
           ;; during eval
           ;;           (is (let [cell (apply (eval (parse-rule "if altitude is less than 100 and state is forest then state should be climax and deer should be 3"))
           ;;                                 (list {:state :forest :altitude 99} nil))]
           ;;                 (and (= (:state cell) :climax) (= (:deer cell) 3))))
           ))
          
