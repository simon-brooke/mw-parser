(ns mw-parser.insta
  (:use mw-engine.utils
        [clojure.string :only [split trim triml]])
  (:require [instaparse.core :as insta]))

;; This is the 'next generation' parser - it is a much better parser than
;; mw-parser.core, but it doesn't completely work yet.

(def grammar
  ;; in order to simplify translation into other natural languages, all 
  ;; TOKENS within the parser should be unambiguous
  "RULE := IF SPACE CONDITIONS SPACE THEN SPACE ACTIONS;
   CONDITIONS := DISJUNCT-CONDITION | CONJUNCT-CONDITION | PROPERTY-CONDITION | NEIGHBOURS-CONDITION ;
   DISJUNCT-CONDITION := CONDITION SPACE OR SPACE CONDITIONS;
   CONJUNCT-CONDITION := CONDITION SPACE AND SPACE CONDITIONS;
   CONDITION := NEIGHBOURS-CONDITION | PROPERTY-CONDITION;
   NEIGHBOURS-CONDITION := QUANTIFIER SPACE NEIGHBOURS SPACE IS SPACE PROPERTY-CONDITION | QUANTIFIER SPACE NEIGHBOURS IS EXPRESSION | QUALIFIER SPACE NEIGHBOURS-CONDITION;
   PROPERTY-CONDITION := PROPERTY SPACE QUALIFIER SPACE EXPRESSION;
   EXPRESSION := SIMPLE-EXPRESSION | RANGE-EXPRESSION | NUMERIC-EXPRESSION | DISJUNCT-EXPRESSION | VALUE;
   SIMPLE-EXPRESSION := QUALIFIER SPACE EXPRESSION | VALUE;
   DISJUNCT-EXPRESSION := IN SPACE DISJUNCT-VALUE;
   RANGE-EXPRESSION := BETWEEN SPACE NUMERIC-EXPRESSION SPACE AND SPACE NUMERIC-EXPRESSION;
   NUMERIC-EXPRESSION := VALUE | VALUE SPACE OPERATOR SPACE NUMERIC-EXPRESSION;
   QUALIFIER := COMPARATIVE SPACE THAN | EQUIVALENCE | IS SPACE QUALIFIER;
   QUANTIFIER := NUMBER | SOME | NONE | ALL;
   EQUIVALENCE := IS SPACE EQUAL | EQUAL | IS ;
   COMPARATIVE := MORE | LESS;
   DISJUNCT-VALUE := VALUE | VALUE SPACE OR SPACE DISJUNCT-VALUE;
   IF := 'if';
   THEN := 'then';
   THAN := 'than';
   OR := 'or';
   AND := 'and';
   SOME := 'some';
   NONE := 'no';
   ALL := 'all'
   BETWEEN := 'between';
   IN := 'in';
   MORE := 'more';
   LESS := 'less' | 'fewer';
   OPERATOR := '+' | '-' | '*' | '/';
   NEIGHBOURS := 'neighbour' | 'neighbor' | 'neighbours' | 'neighbors';
   PROPERTY := SYMBOL;
   VALUE := SYMBOL | NUMBER;
   EQUAL := 'equal to';
   IS := 'is' | 'are' | 'have' | 'has';
   NUMBER := #'[0-9]+' | #'[0-9]+.[0-9]+';
   SYMBOL := #'[a-z]+';
   ACTIONS := ACTION | ACTION SPACE 'and' SPACE ACTIONS
   ACTION := SIMPLE-ACTION | PROBABLE-ACTION;
   PROBABLE-ACTION := VALUE SPACE 'chance in' SPACE VALUE SPACE SIMPLE-ACTION;
   SIMPLE-ACTION := SYMBOL SPACE 'should be' SPACE EXPRESSION
   SPACE := #' *'"
  )

(defn TODO
  "Marker to indicate I'm not yet finished!"
  [message]
  message)


(declare generate simplify)

(defn generate-rule
  "From this `tree`, assumed to be a syntactically correct rule specification,
  generate and return the appropriate rule as a function of two arguments."
  [tree]
  (let [left (generate (nth tree 2))
        right (generate (nth tree 4))]
    (list 'fn ['cell 'world] (list 'if left right))))

(defn generate-conditions
  "From this `tree`, assumed to be a syntactically correct conditions clause,
  generate and return the appropriate clojure fragment."
  [tree]
  (generate (nth tree 1)))

(defn generate-condition
  [tree]
  (generate (nth tree 1)))

(defn generate-conjunct-condition
  [tree]
  (list 'and (generate (nth tree 1))(generate (nth tree 3))))

(defn generate-disjunct-condition
  [tree]
  (list 'or (generate (nth tree 1))(generate (nth tree 3))))


(defn generate-property-condition
  [tree]
  (let [property (generate (nth tree 1))
        qualifier (generate (nth tree 2))
        expression (generate (nth tree 3))]
    (list qualifier (list property 'cell) expression)))

(defn generate-simple-action
  [tree]
  (let [property (generate (nth tree 1))
        expression (generate (nth tree 3))]
    (list 'merge 'cell {property expression})))

(defn generate
  "Generate code for this (fragment of a) parse tree"
  [tree]
  (if
    (coll? tree)
    (case (first tree)
      :RULE (generate-rule tree)
      :CONDITIONS (generate-conditions tree)
      :CONDITION (generate-condition tree)
      ;;    :NEIGHBOURS-CONDITION (generate-neighbours-condition tree)
      :DISJUNCT-CONDITION (generate-disjunct-condition tree)
      :CONJUNCT-CONDITION (generate-conjunct-condition tree)
      :PROPERTY-CONDITION (generate-property-condition tree)
      :SIMPLE-ACTION (generate-simple-action tree)
      :SYMBOL (keyword (second tree))
      :NUMBER (read-string (second tree))
      :EQUIVALENCE '=
      :MORE '>
      :LESS '<
      ;;    :EXPRESSION (generate-expression tree)
      ;;    :SIMPLE-EXPRESSION
      (map generate tree))
    tree))


(defn simplify-qualifier
  "Given that this `tree` fragment represents a qualifier, what 
   qualifier is that?"
  [tree]
  (cond 
    (empty? tree) nil
    (and (coll? tree) 
         (member? (first tree) '(:EQUIVALENCE :COMPARATIVE))) tree
    (coll? (first tree)) (or (simplify-qualifier (first tree))
                             (simplify-qualifier (rest tree)))
    (coll? tree) (simplify-qualifier (rest tree))
    true tree))

(defn simplify-second-of-two
  "There are a number of possible simplifications such that if the `tree` has
   only two elements, the second is semantically sufficient."
  [tree]
  (if (= (count tree) 2) (simplify (nth tree 1)) tree))
  

(defn simplify
  "Simplify/canonicalise this `tree`. Opportunistically replace complex fragments with
  semantically identical simpler fragments"
  [tree]
  (if 
    (coll? tree)
    (case (first tree)
      :SPACE nil
      :QUALIFIER (simplify-qualifier tree)
      :CONDITIONS (simplify-second-of-two tree)
      :CONDITION (simplify-second-of-two tree)
      :EXPRESSION (simplify-second-of-two tree)
      :COMPARATIVE (simplify-second-of-two tree)
      :QUANTIFIER (simplify-second-of-two tree)
      :VALUE (simplify-second-of-two tree)
      :PROPERTY (simplify-second-of-two tree)
      :ACTIONS (simplify-second-of-two tree)
      :ACTION (simplify-second-of-two tree)
      (remove nil? (map simplify tree)))
    tree))

(def parse-rule
  (insta/parser grammar))

(defn compile-rule 
  [rule]
  nil)
;;  (generate (prune-tree (parse-rule rule))))




(compile-rule "if state is climax and some neighbours have state equal to fire then 3 chance in 5 state should be fire")


(compile-rule "if state is in grassland or pasture or heath and 4 neighbours have state equal to water then state should be village")

(compile-rule "if 6 neighbours have state equal to water then state should be village")

(compile-rule "if fertility is between 55 and 75 then state should be climax")

(compile-rule "if state is forest then state should be climax")


(compile-rule "if state is in grassland or pasture or heath and 4 neighbours have state equal to water then state should be village")
(compile-rule "if altitude is less than 100 and state is forest then state should be climax and deer should be 3")
(compile-rule "if altitude is 100 or fertility is 25 then state should be heath and fertility should be 24.3")
(compile-rule "if altitude is 100 or fertility is 25 then state should be heath")

(compile-rule "if deer is more than 2 and wolves is 0 and fertility is more than 20 then deer should be deer + 2")
(compile-rule "if deer is more than 1 and wolves is more than 1 then deer should be deer - wolves")
(compile-rule "if state is grassland and 4 neighbours have state equal to water then state should be village")
