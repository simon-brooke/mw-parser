(ns mw-parser.declarative
  (:use mw-engine.utils
        [clojure.string :only [split trim triml]])
  (:require [instaparse.core :as insta]))


;; error thrown when an attempt is made to set a reserved property
(def reserved-properties-error
  "The properties 'x' and 'y' of a cell are reserved and should not be set in rule actions")
;; error thrown when a rule cannot be parsed. Slots are for
;; (1) rule text
;; (2) cursor showing where in the rule text the error occurred
;; (3) the reason for the error
(def bad-parse-error "I did not understand:\n'%s'\n%s\n%s")


(def grammar
  ;; in order to simplify translation into other natural languages, all
  ;; TOKENS within the parser should be unambiguous
  "RULE := IF SPACE CONDITIONS SPACE THEN SPACE ACTIONS;
   CONDITIONS := DISJUNCT-CONDITION | CONJUNCT-CONDITION | PROPERTY-CONDITION | NEIGHBOURS-CONDITION ;
   DISJUNCT-CONDITION := CONDITION SPACE OR SPACE CONDITIONS;
   CONJUNCT-CONDITION := CONDITION SPACE AND SPACE CONDITIONS;
   CONDITION := NEIGHBOURS-CONDITION | PROPERTY-CONDITION;
   WITHIN-CONDITION := NEIGHBOURS-CONDITION SPACE WITHIN SPACE NUMERIC-EXPRESSION;
   NEIGHBOURS-CONDITION := WITHIN-CONDITION | QUANTIFIER SPACE NEIGHBOURS SPACE IS SPACE PROPERTY-CONDITION | QUANTIFIER SPACE NEIGHBOURS IS EXPRESSION | QUALIFIER SPACE NEIGHBOURS-CONDITION;
   PROPERTY-CONDITION := PROPERTY SPACE QUALIFIER SPACE EXPRESSION;
   EXPRESSION := SIMPLE-EXPRESSION | RANGE-EXPRESSION | NUMERIC-EXPRESSION | DISJUNCT-EXPRESSION | VALUE;
   SIMPLE-EXPRESSION := QUALIFIER SPACE EXPRESSION | VALUE;
   DISJUNCT-EXPRESSION := IN SPACE DISJUNCT-VALUE;
   RANGE-EXPRESSION := BETWEEN SPACE NUMERIC-EXPRESSION SPACE AND SPACE NUMERIC-EXPRESSION;
   NUMERIC-EXPRESSION := VALUE | VALUE SPACE OPERATOR SPACE NUMERIC-EXPRESSION;
   NEGATED-QUALIFIER := QUALIFIER SPACE NOT | NOT SPACE QUALIFIER;
   COMPARATIVE-QUALIFIER := IS SPACE COMPARATIVE SPACE THAN;
   QUALIFIER := COMPARATIVE-QUALIFIER | NEGATED-QUALIFIER | EQUIVALENCE | IS SPACE QUALIFIER;
   QUANTIFIER := NUMBER | SOME | NONE | ALL | COMPARATIVE SPACE THAN SPACE NUMBER;
   EQUIVALENCE := IS SPACE EQUAL | EQUAL | IS ;
   COMPARATIVE := MORE | LESS;
   DISJUNCT-VALUE := VALUE | VALUE SPACE OR SPACE DISJUNCT-VALUE;
   IF := 'if';
   THEN := 'then';
   THAN := 'than';
   OR := 'or';
   NOT := 'not';
   AND := 'and';
   SOME := 'some';
   NONE := 'no';
   ALL := 'all'
   BETWEEN := 'between';
   WITHIN := 'within';
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
   SIMPLE-ACTION := SYMBOL SPACE BECOMES SPACE EXPRESSION
   BECOMES := 'should be'
   SPACE := #' *'"
  )

(defn TODO
  "Marker to indicate I'm not yet finished!"
  [message]
  message)


(declare generate simplify)

(defn suitable-fragment?
  "Return `true` if `tree-fragment` appears to be a tree fragment of the expected `type`."
  [tree-fragment type]
  (and (coll? tree-fragment)(= (first tree-fragment) type)))

(defn assert-type
  "If `tree-fragment` is not a tree fragment of the expected `type`, throw an exception."
  [tree-fragment type]
  (assert (suitable-fragment? tree-fragment type)
          (throw (Exception. (format "Expected a %s fragment" type)))))

(defn generate-rule
  "From this `tree`, assumed to be a syntactically correct rule specification,
  generate and return the appropriate rule as a function of two arguments."
  [tree]
  (assert-type tree :RULE)
  (list 'fn ['cell 'world] (list 'if (generate (nth tree 2)) (generate (nth tree 3)))))

(defn generate-conditions
  "From this `tree`, assumed to be a syntactically correct conditions clause,
  generate and return the appropriate clojure fragment."
  [tree]
  (assert-type tree :CONDITIONS)
  (generate (nth tree 1)))

(defn generate-condition
  [tree]
  (assert-type tree :CONDITION)
  (generate (nth tree 1)))

(defn generate-conjunct-condition
  [tree]
  (assert-type tree :CONJUNCT-CONDITION)
  (list 'and (generate (nth tree 1))(generate (nth tree 3))))

(defn generate-disjunct-condition
  [tree]
  (assert-type tree :DISJUNCT-CONDITION)
  (list 'or (generate (nth tree 1))(generate (nth tree 3))))

(defn generate-ranged-property-condition
  "Generate a property condition where the expression is a numeric range"
  [tree property expression]
  (assert-type tree :PROPERTY-CONDITION)
  (assert-type (nth tree 3) :RANGE-EXPRESSION)
  (let [l1 (generate (nth expression 2))
        l2 (generate (nth expression 4))
        pv (list property 'cell)]
    (list 'let ['lower (list 'min l1 l2)
                'upper (list 'max l1 l2)]
          (list 'and (list '>= pv 'lower)(list '<= pv 'upper)))))

(defn generate-disjunct-condition
  "Generate a property condition where the expression is a disjunct expression"
  [tree property qualifier expression]
  (let [e (list 'some (list 'fn ['i] '(= i value)) (list 'quote expression))]
    (list 'let ['value (list property 'cell)]
          (if (= qualifier '=) e
            (list 'not e)))))

(defn generate-property-condition
  ([tree]
   (assert-type tree :PROPERTY-CONDITION)
   (generate-property-condition tree (first (nth tree 3))))
  ([tree expression-type]
   (assert-type tree :PROPERTY-CONDITION)
   (let [property (generate (nth tree 1))
         qualifier (generate (nth tree 2))
         expression (generate (nth tree 3))]
     (case expression-type
       :DISJUNCT-EXPRESSION (generate-disjunct-condition tree property qualifier expression)
       :RANGE-EXPRESSION (generate-ranged-property-condition tree property expression)
       (list qualifier (list property 'cell) expression)))))

(defn generate-simple-action
  [tree]
  (assert-type tree :SIMPLE-ACTION)
  (let [property (generate (nth tree 1))
        expression (generate (nth tree 3))]
    (if (or (= property :x) (= property :y))
      (throw (Exception. reserved-properties-error))
      (list 'merge 'cell {property expression}))))

(defn generate-multiple-actions
   [tree]
  nil)
;;   (assert (and (coll? tree)(= (first tree) :ACTIONS)) "Expected an ACTIONS fragment")
;;   (conj 'do (map

(defn generate-disjunct-value
  "Generate a disjunct value. Essentially what we need here is to generate a
  flat list of values, since the `member` has already been taken care of."
  [tree]
  (assert-type tree :DISJUNCT-VALUE)
  (if (= (count tree) 4)
    (cons (generate (second tree)) (generate (nth tree 3)))
    (list (generate (second tree)))))

(defn generate-numeric-expression
  [tree]
  (assert-type tree :NUMERIC-EXPRESSION)
  (case (first (second tree))
    :SYMBOL (list (keyword (second (second tree))) 'cell)
    (generate (second tree))))

(defn generate-neighbours-condition
  "Generate code for a condition which refers to neighbours."
  ([tree]
   (generate-neighbours-condition tree (first (second tree))))
  ([tree quantifier-type]
   (let [quantifier (second (second tree))
         pc (generate (nth tree 4))]
     (case quantifier-type
       :NUMBER (generate-neighbours-condition '= (read-string quantifier) pc 1)
       :SOME (generate-neighbours-condition '> 0 pc 1)
       :QUANTIFIER
       (let [comparative (generate (simplify (second quantifier)))
             value (simplify (nth quantifier 5))]
         (generate-neighbours-condition comparative value pc 1)))))
  ([comp1 quantity property-condition distance]
   (list comp1
         (list 'count (list 'remove false (list 'map (list 'fn ['cell] property-condition) '(get-neighbours cell world distance)))) quantity))
  ([comp1 quantity property-condition]
   (generate-neighbours-condition comp1 quantity property-condition 1)))

;; (def s1 "if 3 neighbours have state equal to forest then state should be forest")
;; (def s2 "if some neighbours have state equal to forest then state should be forest")
;; (def s3 "if more than 3 neighbours have state equal to forest then state should be forest")
;; (def s4 "if fewer than 3 neighbours have state equal to forest then state should be forest")
;; (def s5 "if all neighbours have state equal to forest then state should be forest")
;; (def s6 "if more than 3 neighbours within 2 have state equal to forest then state should be forest")

;; (nth (simplify (parse-rule s1)) 2)
;; (second (nth (simplify (parse-rule s1)) 2))
;; (nth (simplify (parse-rule s2)) 2)
;; (map simplify (nth (simplify (parse-rule s2)) 2))
;; ;; (second (nth (simplify (parse-rule s2)) 2))
;; ;; (nth (simplify (parse-rule s3)) 2)
;; (second (nth (simplify (parse-rule s3)) 2))
;; (map simplify (second (nth (simplify (parse-rule s3)) 2)))
;; ;; (nth (simplify (parse-rule s4)) 2)
;; ;; (second (nth (simplify (parse-rule s4)) 2))
;; ;; (nth (simplify (parse-rule s5)) 2)
;; ;; (second (nth (simplify (parse-rule s5)) 2))
;; ;; (nth (simplify (parse-rule s6)) 2)
;; ;; (second (nth (simplify (parse-rule s6)) 2))

;; ;; (generate (nth (nth (simplify (parse-rule s5)) 2) 4))
;; ;; (generate (nth (simplify (parse-rule s2)) 2))
;; ;; (generate (nth (simplify (parse-rule s1)) 2))


;; (generate-neighbours-condition '= 3 '(= (:state cell) :forest) 1)
;; (generate-neighbours-condition (nth (simplify (parse-rule s3)) 2))
;; (generate-neighbours-condition (nth (simplify (parse-rule s2)) 2))
;; (generate-neighbours-condition (nth (simplify (parse-rule s1)) 2))


(defn generate
  "Generate code for this (fragment of a) parse tree"
  [tree]
  (if
    (coll? tree)
    (case (first tree)
      :ACTIONS (generate-multiple-actions tree)
      :COMPARATIVE (generate (second tree))
      :COMPARATIVE-QUALIFIER (generate (nth tree 2))
      :CONDITION (generate-condition tree)
      :CONDITIONS (generate-conditions tree)
      :CONJUNCT-CONDITION (generate-conjunct-condition tree)
      :DISJUNCT-CONDITION (generate-disjunct-condition tree)
      :DISJUNCT-EXPRESSION (generate (nth tree 2))
      :DISJUNCT-VALUE (generate-disjunct-value tree)
      :EQUIVALENCE '=
      :EXPRESSION (generate (second tree))
      :LESS '<
      :MORE '>
      :NEGATED-QUALIFIER (case (generate (second tree))
                                 = 'not=
                                 > '<
                                 < '>)
      :NEIGHBOURS-CONDITION (generate-neighbours-condition tree)
      :NUMERIC-EXPRESSION (generate-numeric-expression tree)
      :NUMBER (read-string (second tree))
      :PROPERTY (list (generate (second tree)) 'cell) ;; dubious - may not be right
      :PROPERTY-CONDITION (generate-property-condition tree)
      :QUALIFIER (generate (second tree))
      :RULE (generate-rule tree)
      :SIMPLE-ACTION (generate-simple-action tree)
      :SYMBOL (keyword (second tree))
      :VALUE (generate (second tree))
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


(defn rule?
  "Return true if the argument appears to be a parsed rule tree, else false."
  [maybe-rule]
  (and (coll? maybe-rule) (= (first maybe-rule) :RULE)))

(defn simplify
  "Simplify/canonicalise this `tree`. Opportunistically replace complex fragments with
  semantically identical simpler fragments"
  [tree]
  (if
    (coll? tree)
    (case (first tree)
      :ACTION (simplify-second-of-two tree)
      :ACTIONS (simplify-second-of-two tree)
      :COMPARATIVE (simplify-second-of-two tree)
      :CONDITION (simplify-second-of-two tree)
      :CONDITIONS (simplify-second-of-two tree)
      :EXPRESSION (simplify-second-of-two tree)
;;      :QUANTIFIER (simplify-second-of-two tree)
      :NOT nil
      :PROPERTY (simplify-second-of-two tree)
      :SPACE nil
      :THEN nil
      ;; :QUALIFIER (simplify-qualifier tree)
      :VALUE (simplify-second-of-two tree)
      (remove nil? (map simplify tree)))
    tree))

(def parse-rule
  "Parse the argument, assumed to be a string in the correct syntax, and return a parse tree."
  (insta/parser grammar))

(defn explain-parse-error-reason
  "Attempt to explain the reason for the parse error."
  [reason]
  (str "Expecting one of (" (apply str (map #(str (:expecting %) " ") (first reason))) ")"))

(defn throw-parse-exception
  "Construct a helpful error message from this `parser-error`, and throw an exception with that message."
  [parser-error]
  (assert (coll? parser-error) "Expected a paser error structure?")
  (let
    [
      ;; the error structure is a list, such that each element is a list of two items, and
      ;; the first element in each sublist is a keyword. Easier to work with it as a map
     error-map (reduce (fn [map item](merge map {(first item)(rest item)})) {} parser-error)
     text (first (:text error-map))
     reason (explain-parse-error-reason (:reason error-map))
      ;; rules have only one line, by definition; we're interested in the column
     column (if (:column error-map)(first (:column error-map)) 0)
      ;; create a cursor to point to that column
     cursor (apply str (reverse (conj (repeat column " ") "^")))
     message (format bad-parse-error text cursor reason)
     ]
  (throw (Exception. message))))

(defn compile-rule
  "Compile this `rule`, assumed to be a string with appropriate syntax, into a function of two arguments,
  a `cell` and a `world`, having the same semantics."
  [rule]
  (assert (string? rule))
  (let [tree (simplify (parse-rule rule))]
    (if (rule? tree) (eval (generate tree))
      (throw-parse-exception tree))))


