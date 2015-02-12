(ns mw-parser.insta
  (:use mw-engine.utils
        [clojure.string :only [split trim triml]])
  (:require [instaparse.core :as insta]))


(def grammar
  "RULE := 'if' SPACE CONDITIONS SPACE 'then' SPACE ACTIONS;
   CONDITIONS := DISJUNCT-CONDITION | CONJUNCT-CONDITION | PROPERTY-CONDITION | NEIGHBOURS-CONDITION ;
   DISJUNCT-CONDITION := CONDITION SPACE 'or' SPACE CONDITIONS;
   CONJUNCT-CONDITION := CONDITION SPACE 'and' SPACE CONDITIONS;
   CONDITION := NEIGHBOURS-CONDITION | PROPERTY-CONDITION;
   NEIGHBOURS-CONDITION := QUANTIFIER SPACE NEIGHBOURS SPACE IS SPACE PROPERTY-CONDITION | QUANTIFIER SPACE NEIGHBOURS IS EXPRESSION | QUALIFIER SPACE NEIGHBOURS-CONDITION;
   PROPERTY-CONDITION := PROPERTY SPACE QUALIFIER SPACE EXPRESSION;
   EXPRESSION := SIMPLE-EXPRESSION | RANGE-EXPRESSION | NUMERIC-EXPRESSION | DISJUNCT-EXPRESSION | VALUE;
   SIMPLE-EXPRESSION := QUALIFIER SPACE EXPRESSION | VALUE;
   DISJUNCT-EXPRESSION := 'in' SPACE DISJUNCT-VALUE;
   RANGE-EXPRESSION := 'between' SPACE NUMERIC-EXPRESSION SPACE 'and' SPACE NUMERIC-EXPRESSION;
   NUMERIC-EXPRESSION := VALUE | VALUE SPACE OPERATOR SPACE NUMERIC-EXPRESSION;
   QUALIFIER := COMPARATIVE SPACE 'than' | EQUIVALENCE | IS SPACE QUALIFIER;
   NEIGHBOURS := 'neighbour' | 'neighbor' | 'neighbours' | 'neighbors';
   QUANTIFIER := NUMBER | 'some' | 'no' | 'all';
   EQUIVALENCE := IS SPACE 'equal to' | 'equal to' | IS ;
   COMPARATIVE := 'more' | 'less' | 'fewer';
   OPERATOR := '+' | '-' | '*' | '/';
   PROPERTY := SYMBOL;
   DISJUNCT-VALUE := VALUE | VALUE SPACE 'or' SPACE DISJUNCT-VALUE;
   VALUE := SYMBOL | NUMBER;
   IS := 'is' | 'are' | 'have';
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


(declare generate)

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

(defn generate-qualifier
  "Return more than (>), less than (<) or equal to (=) depending on the `qualifier`."
  [qualifier]
  (TODO "not written yet")
  tree)


(defn generate-property-condition
  [tree]
  (let [property (generate (nth tree 1))
        qualifier (generate (nth tree 2))
        expression (generate (nth tree 3))]
    (list qualifier (list (keyword property) 'cell) expression)))


(defn generate
  "Generate code for this (fragment of a) parse tree"
  [tree]
  (case (first tree)
    :RULE (generate-rule tree)
    :CONDITIONS (generate-conditions tree)
    :CONDITION (generate-condition tree)
;;    :NEIGHBOURS-CONDITION (generate-neighbours-condition tree)
    :DISJUNCT-CONDITION (generate-disjunct-condition tree)
    :CONJUNCT-CONDITION (generate-conjunct-condition tree)
    :PROPERTY-CONDITION (generate-property-condition tree)
;;    :EXPRESSION (generate-expression tree)
;;    :SIMPLE-EXPRESSION
    tree))

(defn prune-tree
  "Simplify/canonicalise the `tree`. Opportunistically replace complex fragments with
  semantically identical simpler fragments"
  [tree]
  (TODO "not written yet")
  tree)

(defn clean-tree
  "Returns a structure which is structurally equivalent to `tree` but which has
  the noise tokens (spaces) removed. As a side effect this new structure is a
  list, not a vector, but that is not a desideratum and you should not rely in it."
  [tree]
  (cond
   (and (coll? tree) (= (first tree) :SPACE)) nil
   (coll? tree) (remove nil? (map clean-tree tree))
   true tree))

(def rule-parser
  (insta/parser grammar))

(defn compile-rule [rule]
  (generate (prune-tree (clean-tree (rule-parser rule)))))




(compile-rule "if state is climax and some neighbours have state equal to fire then 3 chance in 5 state should be fire")


(rule-parser "if state is in grassland or pasture or heath and 4 neighbours have state equal to water then state should be village")

(rule-parser "if 6 neighbours have state equal to water then state should be village")

(rule-parser "if fertility is between 55 and 75 then state should be climax")

(rule-parser "if state is forest then state should be climax")


(rule-parser "if state is in grassland or pasture or heath and 4 neighbours have state equal to water then state should be village")
(rule-parser "if altitude is less than 100 and state is forest then state should be climax and deer should be 3")
(rule-parser "if altitude is 100 or fertility is 25 then state should be heath and fertility should be 24.3")
(rule-parser "if altitude is 100 or fertility is 25 then state should be heath")

(rule-parser "if deer is more than 2 and wolves is 0 and fertility is more than 20 then deer should be deer + 2")
(rule-parser "if deer is more than 1 and wolves is more than 1 then deer should be deer - wolves")
(rule-parser "if state is grassland and 4 neighbours have state equal to water then state should be village")
