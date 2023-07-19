(ns ^{:doc "A very simple parser which parses production rules."
      :author "Simon Brooke"}
 mw-parser.declarative
  (:require [clojure.string :refer [join split split-lines trim]]
            [instaparse.core :refer [parser]]
            [mw-parser.flow :refer [flow-grammar]]
            [mw-parser.generate :refer [generate]]
            [mw-parser.simplify :refer [simplify]]
            [mw-parser.utils :refer [comment?]]
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

(def rule-grammar
  "Basic rule language grammar.
   
  in order to simplify translation into other natural languages, all
  TOKENS within the parser should be unambiguou."
  (join "\n" ["RULE := IF SPACE CONDITIONS SPACE THEN SPACE ACTIONS;"
              "ACTIONS := ACTION | ACTION SPACE AND SPACE ACTIONS"
              "ACTION := SIMPLE-ACTION | PROBABLE-ACTION;"
              "PROBABLE-ACTION := VALUE SPACE CHANCE-IN SPACE VALUE SPACE SIMPLE-ACTION;"
              "SIMPLE-ACTION := SYMBOL SPACE BECOMES SPACE EXPRESSION;"]))

(def common-grammar
  "Grammar rules used both in the rule grammar and in the flow grammar"
  (join "\n" ["COMPARATIVE := MORE | LESS;"
              "COMPARATIVE-QUALIFIER := IS SPACE COMPARATIVE SPACE THAN | COMPARATIVE SPACE THAN;"
              "CONDITION := WITHIN-CONDITION | NEIGHBOURS-CONDITION | PROPERTY-CONDITION;"
              "CONDITIONS := DISJUNCT-CONDITION | CONJUNCT-CONDITION | CONDITION ;"
              "CONJUNCT-CONDITION := CONDITION SPACE AND SPACE CONDITIONS;"
              "DISJUNCT-CONDITION := CONDITION SPACE OR SPACE CONDITIONS;"
              "DISJUNCT-EXPRESSION := IN SPACE DISJUNCT-VALUE;"
              "DISJUNCT-VALUE := VALUE | VALUE SPACE OR SPACE DISJUNCT-VALUE;"
              "EQUIVALENCE := IS SPACE EQUAL | EQUAL | IS ;"
              "EXPRESSION := SIMPLE-EXPRESSION | RANGE-EXPRESSION | NUMERIC-EXPRESSION | DISJUNCT-EXPRESSION | VALUE;"
              "NEGATED-QUALIFIER := QUALIFIER SPACE NOT | NOT SPACE QUALIFIER;"
              "NEIGHBOURS-CONDITION := QUANTIFIER SPACE NEIGHBOURS SPACE IS SPACE PROPERTY-CONDITION | QUALIFIER SPACE NEIGHBOURS-CONDITION;"
              "NUMBER := #'[0-9]+' | #'[0-9]+.[0-9]+';"
              "NUMERIC-EXPRESSION := VALUE | VALUE SPACE OPERATOR SPACE NUMERIC-EXPRESSION;"
              "OPERATOR := '+' | '-' | '*' | '/';"
              "PROPERTY := SYMBOL;"
              "PROPERTY-CONDITION := PROPERTY SPACE QUALIFIER SPACE EXPRESSION | VALUE;"
              "PROPERTY-CONDITION-OR-EXPRESSION := PROPERTY-CONDITION | EXPRESSION;"
              "QUALIFIER := COMPARATIVE-QUALIFIER | NEGATED-QUALIFIER | EQUIVALENCE | IS SPACE QUALIFIER;"
              "QUANTIFIER := NUMBER | SOME | NONE | ALL | COMPARATIVE SPACE THAN SPACE NUMBER;"
              "RANGE-EXPRESSION := BETWEEN SPACE NUMERIC-EXPRESSION SPACE AND SPACE NUMERIC-EXPRESSION;"
              "SIMPLE-EXPRESSION := QUALIFIER SPACE EXPRESSION | VALUE;"
              "SPACE := #'\\s+';"
              "VALUE := SYMBOL | NUMBER;"
              "VALUE := SYMBOL | NUMBER;"
              "WITHIN-CONDITION := QUANTIFIER SPACE NEIGHBOURS SPACE WITHIN SPACE NUMBER SPACE IS SPACE PROPERTY-CONDITION-OR-EXPRESSION;"]))

(def keywords-en
  "English language keyword literals used in rules - both in production
   rules (this namespace) and in flow rules (see mw-parser.flow).
      
      It's a long term aim that the rule language should be easy to 
      internationalise; this isn't a full solution but it's a step towards
      a solution."
  (join "\n" ["ALL := 'all'"
              "AND := 'and';"
              "BECOMES := 'should be' | 'becomes';"
              "BETWEEN := 'between';"
              "CHANCE-IN := 'chance in';"
              "EACH := 'each' | 'every' | 'all';"
              "EQUAL := 'equal to';"
              "FIRST := 'first';"
              "FLOW := 'flow' | 'move';"
              "FROM := 'from';"
              "IF := 'if';"
              "IN := 'in';"
              "IS := 'is' | 'are' | 'have' | 'has';"
              "LEAST := 'least';"
              "LESS := 'less' | 'fewer';"
              "MORE := 'more' | 'greater';"
              "MOST := 'most';"
              "NEIGHBOURS := 'neighbour' | 'neighbor' | 'neighbours' | 'neighbors';"
              "NONE := 'no';"
              "NOT := 'not';"
              "OR := 'or';"
              "SOME := 'some';"
              ;; SYMBOL is in the per-language file so that languages that use
              ;; (e.g.) Cyrillic characters can change the definition.
              "SYMBOL := #'[a-z]+';"
              "THAN := 'than';"
              "THEN := 'then';"
              "TO := 'to';"
              "WITH := 'with' | 'where' | 'having';"
              "WITHIN := 'within';"]))

(defn keywords-for-locale
  "For now, just return `keywords-en`; plan is to have resource files of 
   keywords for different languages in a resource directory, but that isn't
   done yet. It's probably not going to work easily for languages that use
   non-latin alphabets, anyway."
  ([]
   (keywords-for-locale (get-default)))
  ([^Locale _locale]
   keywords-en))

(defmacro build-parser
  "Compose this grammar fragment `g` with the common grammar fragments to 
   make a complete grammar, and return a parser for that complete grammar."
  [g]
  `(parser (join "\n" [~g common-grammar (keywords-for-locale)])))

(def parse-rule
  "Parse the argument, assumed to be a string in the correct syntax, and return a parse tree."
  (build-parser rule-grammar))

(def parse-flow
  "Parse the argument, assumed to be a string in the correct syntax, and return a parse tree."
  (build-parser flow-grammar))

(defn parse
  "Top level parser function: parse this `text` as either a production or a flow rule; 
   return a raw parse tree."
  [^String rule-text]
  (let [text (trim rule-text)]
    (when-not (zero? (count text))
      (case (first (split text #"\s+"))
        "if" (parse-rule text)
        "flow" (parse-flow text)
        ";;" nil
        (throw (ex-info "Rule text was not recognised" {:text text}))))))

(defn compile
  "Parse this `rule-text`, a string conforming to the grammar of MicroWorld rules,
   into Clojure source, and then compile it into an anonymous
   function object, getting round the problem of binding mw-engine.utils in
   the compiling environment. If `return-tuple?` is present and true, return
   a list comprising the anonymous function compiled, and the function from
   which it was compiled.

   Throws an exception if parsing fails."
  ([rule-text return-tuple?]
   (let [lines (map trim (remove comment? (split-lines rule-text)))]
     (if (> (count lines) 1)
       (map #(compile % return-tuple?) lines)
       (let [src (first lines)
             parse-tree (doall (simplify (parse src)))
             fn' (doall (generate parse-tree))
             afn (try
                   (if (#{'fn 'fn*} (first fn'))
                     (vary-meta (eval fn') merge (meta fn'))
                     (throw (Exception. 
                             (format "Parse of `%s` did not return a function: %s" 
                                     src fn'))))
                   (catch Exception any (throw (ex-info (.getMessage any)
                                                        {:src src
                                                         :parse parse-tree
                                                         :fn fn'}))))]
         (if
          return-tuple?
           (vary-meta (list afn src fn') merge (meta afn))
           afn)))))
  ([rule-text]
   (compile rule-text false)))
