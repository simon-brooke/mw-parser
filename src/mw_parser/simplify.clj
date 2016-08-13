(ns ^{:doc "Simplify a parse tree."
      :author "Simon Brooke"}
  mw-parser.simplify
  (:require [mw-engine.utils :refer [member?]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
;; USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declare simplify)

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


(defn simplify-quantifier
  "If this quantifier is a number, 'simplifiy' it into a comparative whose operator is '='
  and whose quantity is that number. This is actually more complicated but makes generation easier."
  [tree]
  (if (number? (second tree)) [:COMPARATIVE '= (second tree)] (simplify (second tree))))


(defn simplify
  "Simplify/canonicalise this `tree`. Opportunistically replace complex fragments with
  semantically identical simpler fragments"
  [tree]
  (if
    (coll? tree)
    (case (first tree)
      ;; 'all' isn't actually the same as 'eight', because cells at the edges of the world have
      ;; fewer than eight neighbours; but it's a simplifying (ha!) assumption for now."
      ;; TODO: fix this so it actually works.
      :ALL [:COMPARATIVE '= 8]
      :ACTION (simplify-second-of-two tree)
      :ACTIONS (simplify-second-of-two tree)
      :COMPARATIVE (simplify-second-of-two tree)
      :CONDITION (simplify-second-of-two tree)
      :CONDITIONS (simplify-second-of-two tree)
      :EXPRESSION (simplify-second-of-two tree)
      :NONE [:COMPARATIVE '= 0]
      :NUMBER tree
      :PROPERTY (simplify-second-of-two tree)
      :QUALIFIER (simplify-qualifier tree)
      :QUANTIFIER (simplify-quantifier tree)
      :SOME [:COMPARATIVE '> 0]
      :SPACE nil
      :VALUE (simplify-second-of-two tree)
      (remove nil? (map simplify tree)))
    tree))

