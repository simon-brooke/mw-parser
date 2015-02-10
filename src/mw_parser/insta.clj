(ns mw-parser.insta
  (:use mw-engine.utils
        [clojure.string :only [split trim triml]])
  (:require [instaparse.core :as insta]))


(def grammar 
  "RULE := 'if' SPACE CONDITIONS SPACE 'then' SPACE ACTIONS;
   CONDITIONS := CONDITION | CONDITION SPACE 'and' SPACE CONDITIONS;
   CONDITION := DISJUNCT-CONDITION | PROPERTY-CONDITION;
   DISJUNCT-CONDITION := CONDITION SPACE 'or' SPACE CONDITION;
   PROPERTY-CONDITION := PROPERTY SPACE 'is' SPACE EXPRESSION;
   EXPRESSION := VALUE QUALIFIER EXPRESSION | VALUE OPERATOR EXPRESSION | VALUE;
   QUALIFIER := SPACE 'more' SPACE 'than' SPACE | SPACE 'less' SPACE 'than' SPACE | SPACE 'fewer' SPACE 'than' SPACE | SPACE 'equal' SPACE 'to' SPACE ;
   OPERATOR := '+' | '-' | '*' | '/';
   PROPERTY := SYMBOL;
   VALUE := SYMBOL | NUMBER;
   NUMBER := #'[0-9.]+';
   SYMBOL := #'[a-z]+';
   ACTIONS := ACTION | ACTION SPACE 'and' SPACE ACTIONS
   ACTION := SYMBOL SPACE 'should' SPACE 'be' SPACE EXPRESSION
   SPACE := #'[:blank:]*'"
  )

(def rule-parser
  (insta/parser grammar))

(def token-parser (insta/parser "TOKEN := #'[a-z]+'"))


  
  