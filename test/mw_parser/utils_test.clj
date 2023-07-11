(ns mw-parser.utils-test
  (:require [clojure.test :refer [deftest is testing]]
            [mw-parser.utils :refer [assert-type rule? search-tree
                                     suitable-fragment? TODO]]))

(deftest fragment-tests
  (testing "Functions finding and identifying rule fragments"
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
          not-rule [:FROBOZ :foo :bar :ban]]
      (is (rule? rule))
      (is (not (rule? not-rule)))
      (is (= nil (assert-type rule :RULE)))
      (is (thrown-with-msg? 
           Exception #"Expected a :RULE fragment" (assert-type not-rule :RULE)))
      (is (= '(:EQUIVALENCE (:IS "is")) (search-tree rule :EQUIVALENCE)))
      (is (= nil (search-tree rule :EQUIVOCATION)))
      (is (suitable-fragment? '(:EQUIVALENCE (:IS "is")) :EQUIVALENCE))
      (is (not (suitable-fragment? :EQUIVALENCE :EQUIVALENCE)))
      (is (not (suitable-fragment? '(:EQUIVALENCE (:IS "is")) :QUALIFIER)))
      (is (= (TODO "Froboz") "Froboz")))))