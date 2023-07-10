(ns ^{:doc "Simplify a parse tree."
      :author "Simon Brooke"}
  mw-parser.simplify
  (:require [clojure.pprint :refer [pprint]]
            [mw-engine.utils :refer [member?]]))

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

(declare simplify-flow simplify-rule)

;; (defn simplify-qualifier
;;   "Given that this `tree` fragment represents a qualifier, what
;;   qualifier is that?"
;;   [tree]
;;   (cond
;;     (empty? tree) nil
;;     (and (coll? tree)
;;          (#{:EQUIVALENCE :COMPARATIVE} (first tree))) tree
;;     (coll? (first tree)) (or (simplify-qualifier (first tree))
;;                              (simplify-qualifier (rest tree)))
;;     (coll? tree) (simplify-qualifier (rest tree))
;;     :else tree))

(defn simplify-second-of-two
  "There are a number of possible simplifications such that if the `tree` has
  only two elements, the second is semantically sufficient."
  [tree]
  (if (= (count tree) 2) (simplify-rule (nth tree 1)) tree))

;; (defn simplify-quantifier
;;   "If this quantifier is a number, 'simplifiy' it into a comparative whose operator is '='
;;   and whose quantity is that number. This is actually more complicated but makes generation easier."
;;   [tree]
;;   (if (number? (second tree)) [:COMPARATIVE '= (second tree)] (simplify-rule (second tree))))

(defn simplify-rule
  "Simplify/canonicalise this `tree`. Opportunistically replace complex fragments with
  semantically identical simpler fragments"
  [tree]
  (if
    (coll? tree)
    (case (first tree)
      :ACTION (simplify-second-of-two tree)
      :ACTIONS (cons (first tree) (simplify-rule (rest tree)))
      :CHANCE-IN nil
      :COMPARATIVE (simplify-second-of-two tree)
      :CONDITION (simplify-second-of-two tree)
      :CONDITIONS (simplify-second-of-two tree)
      :EXPRESSION (simplify-second-of-two tree)
      :PROPERTY (simplify-second-of-two tree)
      :PROPERTY-CONDITION-OR-EXPRESSION (simplify-second-of-two tree)
      :SPACE nil
      :THEN nil
      :AND nil
      :VALUE (simplify-second-of-two tree)
      (remove nil? (map simplify-rule tree)))
    tree))

(defn simplify-determiner-condition
  [tree])

(defn simplify-flow
  [tree]
  (if (coll? tree)
    (case (first tree)
      :FLOW nil
      :DETERMINER (simplify-second-of-two tree)
      :DETERMINER-CONDITION (simplify-determiner-condition tree)
      :SPACE nil
      :QUANTITY (simplify-second-of-two tree)
      :STATE [:PROPERTY-CONDITION
              [:SYMBOL "state"]
              [:QUALIFIER 
               [:EQUIVALENCE 
                [:IS "is"]]]
              [:EXPRESSION
               [:VALUE
                (second tree)]]]
      (remove nil? (map simplify-flow tree)))
    tree))