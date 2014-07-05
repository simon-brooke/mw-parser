(ns mw-parser.core-test
  (:use mw-engine.utils)
  (:require [clojure.test :refer :all]
            [mw-parser.core :refer :all]))


(deftest rules-tests
  (testing "if altitude is less than 100 and state is forest then state should be climax and deer should be 3"
           (is (parse-rule "if altitude is less than 100 and state is forest then state should be climax and deer should be 3"))
           (is (let [cell (apply (eval (parse-rule "if altitude is less than 100 and state is forest then state should be climax and deer should be 3"))
                                 (list {:state :forest :altitude 99} nil))]
                 (and (= (:state cell) :climax) (= (:deer cell) 3))))
           ))
          

;; * "if altitude is 100 or fertility is 25 then state should be heath and fertility should be 24.3"
;; * "if altitude is 100 or fertility is 25 then state should be heath"
;; * "if deer is more than 2 and wolves is 0 and fertility is more than 20 then deer should be deer + 2"
;; * "if deer is more than 1 and wolves is more than 1 then deer should be deer - wolves"
;;