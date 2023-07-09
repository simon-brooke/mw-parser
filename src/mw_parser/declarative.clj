(ns ^{:doc "A very simple parser which parses production rules."
      :author "Simon Brooke"}
 mw-parser.declarative
  (:require [instaparse.core :as insta]
            [clojure.string :refer [join trim]]
            [mw-parser.errors :refer [throw-parse-exception]]
            [mw-parser.generate :refer [generate]]
            [mw-parser.simplify :refer [simplify]]
            [mw-parser.utils :refer [rule?]]
            [trptr.java-wrapper.locale :refer [get-default]])
  (:import [java.util Locale]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; mw-parser: a rule parser for MicroWorld.
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License
;;;; as published by the Free Software Foundation; either version 2
;;;; of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
;;;; USA.
;;;;
;;;; Copyright (C) 2014 Simon Brooke
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def grammar
  "Basic rule language grammar.
   
  in order to simplify translation into other natural languages, all
  TOKENS within the parser should be unambiguou."
  (join "\n" ["RULE := IF SPACE CONDITIONS SPACE THEN SPACE ACTIONS;"
              "CONDITIONS := DISJUNCT-CONDITION | CONJUNCT-CONDITION | CONDITION ;"
              "DISJUNCT-CONDITION := CONDITION SPACE OR SPACE CONDITIONS;"
              "CONJUNCT-CONDITION := CONDITION SPACE AND SPACE CONDITIONS;"
              "CONDITION := WITHIN-CONDITION | NEIGHBOURS-CONDITION | PROPERTY-CONDITION;"
              "WITHIN-CONDITION := QUANTIFIER SPACE NEIGHBOURS SPACE WITHIN SPACE NUMBER SPACE IS SPACE PROPERTY-CONDITION-OR-EXPRESSION;"
              "NEIGHBOURS-CONDITION := QUANTIFIER SPACE NEIGHBOURS SPACE IS SPACE PROPERTY-CONDITION | QUALIFIER SPACE NEIGHBOURS-CONDITION;"
              "PROPERTY-CONDITION-OR-EXPRESSION := PROPERTY-CONDITION | EXPRESSION;"
              "PROPERTY-CONDITION := PROPERTY SPACE QUALIFIER SPACE EXPRESSION | VALUE;"
              "EXPRESSION := SIMPLE-EXPRESSION | RANGE-EXPRESSION | NUMERIC-EXPRESSION | DISJUNCT-EXPRESSION | VALUE;"
              "SIMPLE-EXPRESSION := QUALIFIER SPACE EXPRESSION | VALUE;"
              "DISJUNCT-EXPRESSION := IN SPACE DISJUNCT-VALUE;"
              "RANGE-EXPRESSION := BETWEEN SPACE NUMERIC-EXPRESSION SPACE AND SPACE NUMERIC-EXPRESSION;"
              "NUMERIC-EXPRESSION := VALUE | VALUE SPACE OPERATOR SPACE NUMERIC-EXPRESSION;"
              "NEGATED-QUALIFIER := QUALIFIER SPACE NOT | NOT SPACE QUALIFIER;"
              "COMPARATIVE-QUALIFIER := IS SPACE COMPARATIVE SPACE THAN | COMPARATIVE SPACE THAN;"
              "QUALIFIER := COMPARATIVE-QUALIFIER | NEGATED-QUALIFIER | EQUIVALENCE | IS SPACE QUALIFIER;"
              "QUANTIFIER := NUMBER | SOME | NONE | ALL | COMPARATIVE SPACE THAN SPACE NUMBER;"
              "EQUIVALENCE := IS SPACE EQUAL | EQUAL | IS ;"
              "COMPARATIVE := MORE | LESS;"
              "DISJUNCT-VALUE := VALUE | VALUE SPACE OR SPACE DISJUNCT-VALUE;"
              "PROPERTY := SYMBOL;"
              "VALUE := SYMBOL | NUMBER;"
              "OPERATOR := '+' | '-' | '*' | '/';"
              "NUMBER := #'[0-9]+' | #'[0-9]+.[0-9]+';"
              "ACTIONS := ACTION | ACTION SPACE AND SPACE ACTIONS"
              "ACTION := SIMPLE-ACTION | PROBABLE-ACTION;"
              "PROBABLE-ACTION := VALUE SPACE CHANCE-IN SPACE VALUE SPACE SIMPLE-ACTION;"
              "SIMPLE-ACTION := SYMBOL SPACE BECOMES SPACE EXPRESSION;"
              "SPACE := #'\\s+';"]))

(def keywords-en
  "English language keyword literals used in rules.
      
      It's a long term aim that the rule language should be easy to 
      internationalise; this isn't a full solution but it's a step towards
      a solution."
  (join "\n" ["IF := 'if';"
              "THEN := 'then';"
              "THAN := 'than';"
              "OR := 'or';"
              "NOT := 'not';"
              "AND := 'and';"
              "SOME := 'some';"
              "NONE := 'no';"
              "ALL := 'all'"
              "BETWEEN := 'between';"
              "WITHIN := 'within';"
              "IN := 'in';"
              "MORE := 'more' | 'greater';"
              "LESS := 'less' | 'fewer';"
              "NEIGHBOURS := 'neighbour' | 'neighbor' | 'neighbours' | 'neighbors';"
              "EQUAL := 'equal to';"
              "IS := 'is' | 'are' | 'have' | 'has';"
              "CHANCE-IN := 'chance in';"
              "BECOMES := 'should be' | 'becomes';"
              ;; SYMBOL is in the per-language file so that languages that use
              ;; (e.g.) Cyrillic characters can change the definition.
              "SYMBOL := #'[a-z]+';"
              ]))

(defn select-keywords-for-locale
  "For now, just return `keywords-en`; plan is to have resource files of 
   keywords for different languages in a resource directory, but that isn't
   done yet. It's probably not going to work easily for languages that use
   non-latin alphabets, anyway."
  ([]
   (select-keywords-for-locale (get-default)))
  ([^Locale locale]
   keywords-en))

(def parse-rule
  "Parse the argument, assumed to be a string in the correct syntax, and return a parse tree."
  (insta/parser (join "\n" [grammar (select-keywords-for-locale)])))

(defn compile-rule
  "Parse this `rule-text`, a string conforming to the grammar of MicroWorld rules,
  into Clojure source, and then compile it into an anonymous
  function object, getting round the problem of binding mw-engine.utils in
  the compiling environment. If `return-tuple?` is present and true, return
  a list comprising the anonymous function compiled, and the function from
  which it was compiled.

  Throws an exception if parsing fails."
  ([rule-text return-tuple?]
   (assert (string? rule-text))
   (let [rule (trim rule-text)
         tree (simplify (parse-rule rule))
         afn (if (rule? tree) (eval (generate tree))
               ;; else
                 (throw-parse-exception tree))]
     (if return-tuple?
       (list afn rule)
       ;; else
       afn)))
  ([rule-text]
   (compile-rule rule-text false)))

