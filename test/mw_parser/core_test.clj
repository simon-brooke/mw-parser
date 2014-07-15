(ns mw-parser.core-test
  (:use mw-engine.core 
        mw-engine.utils
        mw-engine.world)
  (:require [clojure.test :refer :all]
            [mw-parser.core :refer :all]))


(deftest rules-tests
  (testing "Rule parser - does not test whether generated functions actually work, just that something is generated!"
           (is (parse-rule "if altitude is less than 100 and state is forest then state should be climax and deer should be 3"))
           (is (parse-rule "if altitude is 100 or fertility is 25 then state should be heath and fertility should be 24.3"))
           (is (parse-rule "if altitude is 100 or fertility is 25 then state should be heath"))
           (is (parse-rule "if deer is more than 2 and wolves is 0 and fertility is more than 20 then deer should be deer + 2"))
           (is (parse-rule "if deer is more than 1 and wolves is more than 1 then deer should be deer - wolves"))
           (is (parse-rule "if state is grassland and 4 neighbours have state equal to water then state should be village"))
           (is (parse-rule "if state is forest and fertility is between 55 and 75 then state should be climax"))
           (is (parse-rule "if 6 neighbours have state equal to water then state should be village"))
           (is (parse-rule "if state is in grassland or pasture or heath and 4 neighbours are water then state should be village"))
     ;;      (is (parse-rule "if state is climax and some neighbours have state is fire then 3 chance in 5 that state should be fire"))
           (is (parse-rule "if state is pasture and more than 3 neighbours have state equal to scrub then state should be scrub"))
           ))

;; ideally should also test that the rule works, but I haven't worked out how 
;; to make mw-engine.utils available during eval          
(deftest generation-tests
  (testing "Code generation"
          (is 
            (do 
              (use 'mw-engine.utils)
              (eval 
                (parse-rule "if altitude is less than 100 and state is forest then state should be climax and deer should be 3"))))))

(deftest correctness-tests
  (testing "Testing that generated code performs as expected."
          (is (let [afn (compile-rule "if altitude is less than 100 and state is forest then state should be climax and deer should be 3")]
                (= (apply afn (list {:state :forest :altitude 99} nil))
                   {:state :climax :altitude 99 :deer 3})))
          (is (let [afn (compile-rule  "if more than 3 neighbours have state equal to new then state should be scrub")]
                (= (transform-world (make-world 3 3) (list afn))
                   '(({:generation 1 :x 0, :y 0, :state :new} 
                       {:generation 1 :x 1, :y 0, :state :scrub} 
                       {:generation 1 :x 2, :y 0, :state :new}) 
                      ({:generation 1 :x 0, :y 1, :state :scrub} 
                        {:generation 1 :x 1, :y 1, :state :scrub} 
                        {:generation 1 :x 2, :y 1, :state :scrub}) 
                      ({:generation 1 :x 0, :y 2, :state :new} 
                        {:generation 1 :x 1, :y 2, :state :scrub} 
                        {:generation 1 :x 2, :y 2, :state :new}))))
              "The 'Keyword cannnot be cast to Number' bug")
          (is (let [afn (compile-rule  "if state is new then fertility should be fertility + 1")]
                (empty? 
                  (remove 
                    #(= % 1) 
                    (map #(:fertility %) 
                         (flatten 
                           (transform-world (make-world 3 3) (list afn)))))))
              "Arithmetic action")

          
          ))
