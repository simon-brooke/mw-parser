(ns ^{:doc "A very simple parser which parses production rules."
      :author "Simon Brooke"}
 mw-parser.declarative
  (:require [clojure.string :refer [join split-lines]]
            [instaparse.core :refer [failure? get-failure parser]]
            [instaparse.failure :refer [pprint-failure]]
            [mw-parser.flow :refer [flow-grammar]]
            [mw-parser.generate :refer [generate]]
            [mw-parser.simplify :refer [simplify]]
            [taoensso.timbre :as l]
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

;;; TODO: Either, when I first wrote this parser, I didn't adequately read the
;;; Instaparse documentation, or Instaparse has advanced considerably since 
;;; then. Reading the documentation now, I could probably rewrite this to
;;; eliminate the simplify step altogether, and that would be well worth doing.

(def ruleset-grammar
  "Experimental: parse a whole file in one go."
  ;; TODO: bug here. We're double-counting (some) blank lines
  (join "\n" ["LINES := (LINE)+;"
              "LINE := RULE <CR> | FLOW-RULE <CR> | COMMENT <CR> | <CR> ;"
              "CR := #'[ \\t]*[\\r\\n][- \\t]*';"
              "COMMENT := #'[;\\#]+[^\\r\\n]*' | #'/\\*.*\\*/'"]))

(def rule-grammar
  "Basic rule language grammar.
   
  in order to simplify translation into other natural languages, all
  TOKENS within the parser should be unambiguous."
  (join "\n" ["RULE := IF <SPACE> CONDITIONS <SPACE> <THEN> <SPACE> ACTIONS;"
              "ACTIONS := ACTION | (ACTION <SPACE> <AND> <SPACE> ACTION)+"
              "ACTION := SIMPLE-ACTION | PROBABLE-ACTION;"
              "PROBABLE-ACTION := VALUE <SPACE> <CHANCE-IN> <SPACE> VALUE <SPACE> SIMPLE-ACTION;"
              "SIMPLE-ACTION := SYMBOL <SPACE> BECOMES <SPACE> EXPRESSION;"]))

(def common-grammar
  "Grammar rules used both in the rule grammar and in the flow grammar"
  (join "\n" ["COMPARATIVE := MORE | LESS;"
              "COMPARATIVE-QUALIFIER := IS <SPACE> COMPARATIVE <SPACE> THAN | COMPARATIVE <SPACE> THAN;"
              "CONDITION := WITHIN-CONDITION | NEIGHBOURS-CONDITION | PROPERTY-CONDITION;"
              "CONDITIONS := DISJUNCT-CONDITION | CONJUNCT-CONDITION | CONDITION ;"
              "CONJUNCT-CONDITION := CONDITION <SPACE> <AND> <SPACE> CONDITIONS;"
              "DISJUNCT-CONDITION := CONDITION <SPACE> <OR> <SPACE> CONDITIONS;"
              "DISJUNCT-EXPRESSION := <IN> <SPACE> DISJUNCT-VALUE;"
              "DISJUNCT-VALUE := (VALUE <SPACE> <OR> <SPACE>)* VALUE;"
              "EQUIVALENCE := IS <SPACE> EQUAL | EQUAL | IS ;"
              "EXPRESSION := SIMPLE-EXPRESSION | RANGE-EXPRESSION | NUMERIC-EXPRESSION | DISJUNCT-EXPRESSION | VALUE;"
              "NEGATED-QUALIFIER := QUALIFIER <SPACE> NOT | NOT <SPACE> QUALIFIER;"
              "NEIGHBOURS-CONDITION := QUANTIFIER <SPACE> NEIGHBOURS <SPACE> IS <SPACE> PROPERTY-CONDITION | QUALIFIER <SPACE> NEIGHBOURS-CONDITION;"
              "NUMBER := #'[0-9]+' | #'[0-9]+.[0-9]+';"
              "NUMERIC-EXPRESSION := VALUE | VALUE <SPACE> OPERATOR <SPACE> NUMERIC-EXPRESSION;"
              "OPERATOR := '+' | '-' | '*' | '/';"
              "PROPERTY := SYMBOL;"
              "PROPERTY-CONDITION := PROPERTY <SPACE> QUALIFIER <SPACE> EXPRESSION | VALUE;"
              "PROPERTY-CONDITION-OR-EXPRESSION := PROPERTY-CONDITION | EXPRESSION;"
              "QUALIFIER := COMPARATIVE-QUALIFIER | NEGATED-QUALIFIER | EQUIVALENCE | IS <SPACE> QUALIFIER;"
              "QUANTIFIER := NUMBER | SOME | NONE | ALL | COMPARATIVE <SPACE> THAN <SPACE> NUMBER;"
              "RANGE-EXPRESSION := BETWEEN <SPACE> NUMERIC-EXPRESSION <SPACE> AND <SPACE> NUMERIC-EXPRESSION;"
              "SIMPLE-EXPRESSION := QUALIFIER <SPACE> EXPRESSION | VALUE;"
              "SPACE := #'[ \\t]+';"
              "VALUE := SYMBOL | NUMBER;"
              "VALUE := SYMBOL | NUMBER;"
              "WITHIN-CONDITION := QUANTIFIER <SPACE> NEIGHBOURS <SPACE> WITHIN <SPACE> NUMBER <SPACE> IS <SPACE> PROPERTY-CONDITION-OR-EXPRESSION;"]))

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

(def ^:private raw-parser
  (parser (join "\n" [ruleset-grammar rule-grammar flow-grammar common-grammar (keywords-for-locale)])))

(defn parse
  "Parse the argument, assumed to be a string in the correct syntax, and return a parse tree."
  [arg]
  (let [parse-tree-or-error (raw-parser arg :total true)]
    (if (failure? parse-tree-or-error)
      (throw (ex-info (format "Some rules were not understood:\n%s" 
                              (pprint-failure (get-failure parse-tree-or-error)))
                      {:source arg
                       :failure (get-failure parse-tree-or-error)}))
      parse-tree-or-error)))

(defn- compile-rule
  "Compile a rule function from this `parse-tree` derived from this `source`
   at the zero-based line number `n` in the source file; return a compiled
   function, whose metadata has the keys:
   
   * `:rule-type` : the type of rule the function represents;
   * `:parse` : this `parse-tree`;
   * `:source` : the rule source from which the parse tree was derived;
   * `:lisp` : the lisp source generated from this `parse-tree`;
   * `:line : the one-based line number of the definition in the source file,
     i.e. `(inc n)`."
  [parse-tree source n]
  (if (#{:COMMENT :LINE} (first parse-tree))
    (do 
      (l/info (format "Skipping line %d, `%s`, parse-tree %s." 
                      (inc n) source parse-tree))
      nil)
    (let [lisp (generate parse-tree)
          line-no (inc n)]
      (l/info (format "Compiling rule at line %d, `%s`." line-no source))
      (try
        (if (#{'fn 'fn*} (first lisp))
          (vary-meta
           (eval lisp)
           merge (meta lisp) {:source source :lisp lisp :line line-no})
          (throw
           (Exception.
            (format "Parse of `%s` did not return a function: %s" source lisp))))
        (catch Exception any (throw (ex-info (.getMessage any)
                                             {:source source
                                              :parse parse-tree
                                              :lisp lisp
                                              :line line-no})))))))

(defn compile
  "Parse this `rule-text`, a string conforming to the grammar of MicroWorld rules,
   into Clojure source, and then compile it into an anonymous
   function object, getting round the problem of binding mw-engine.utils in
   the compiling environment. 

   Returns a list of anonymous functions each of two arguments, `[cell world]`,
   as expected for a MicroWorld rule function. Each function is decorated with 
   metadata having the keys:
  
   * `:rule-type` : the type of rule the function represents;
   * `:lisp` : the lisp source from which the function was compiled;
   * `:parse` : the parse-tree from which that lisp source was derived;
   * `:source` : the rule source from which the parse-tree was derived;
   * `:line : the one-based line number of the rule source in the source file.
 
   Throws an exception if parsing fails."
  [rule-text]
  (let [lines (split-lines rule-text)]
    (remove
     nil?
     (map
      compile-rule
      (simplify (parse rule-text))
      lines
      (range (count lines))))))