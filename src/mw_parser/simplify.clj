(ns ^{:doc "Simplify a parse tree."
      :author "Simon Brooke"}
 mw-parser.simplify 
  (:require [mw-parser.utils :refer [search-tree]]))

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

(declare simplify)

(defn simplify-second-of-two
  "There are a number of possible simplifications such that if the `tree` has
  only two elements, the second is semantically sufficient."
  [tree]
  (if (= (count tree) 2) (simplify (nth tree 1)) tree))

(defn simplify-chained-list
  "Some parse trees take the form 
   `[:X [:Y 1] :NOISE :NOISE [:X [:Y 2] :NOISE :NOISE [:X [:Y 3]]]]`
   where what's wanted is `[:X [:Y 1] [:Y 2] [:Y 2]]` -- :DISJUNCT-VALUE is a case
   in point. This takes such a parse `tree`, where `branch-tag` is the tag of
   the enclosing form and `leaf-tag` is the tag of the form to be collected, and 
   returns the desired form."
  [tree branch-tag leaf-tag]
  (cons
   (first tree)
   (reverse
    (loop [chain (rest tree) v '()]
      (let [car (first chain)]
        (cond (empty? chain) v
              (coll? car) (let [caar (first car)]
                            (cond
                              (= branch-tag caar) (recur car v)
                              (= leaf-tag caar) (recur
                                                 (rest chain)
                                                 (cons (simplify car) v))
                              :else (recur (rest chain) v)))
              :else (recur (rest chain) v)))))))

(defn simplify
  "Simplify/canonicalise this `tree`. Opportunistically replace complex fragments with
  semantically identical simpler fragments"
  [tree]
  (if
   (coll? tree)
    (case (first tree)
      :ACTION (simplify-second-of-two tree)
      :ACTIONS (cons (first tree) (simplify (rest tree)))
      :AND nil
      :CHANCE-IN nil
      :COMPARATIVE (simplify-second-of-two tree)
      :CONDITION (simplify-second-of-two tree)
      :CONDITIONS (simplify-second-of-two tree)
      :DISJUNCT-EXPRESSION (simplify-chained-list tree :DISJUNCT-VALUE :VALUE)
      :EXPRESSION (simplify-second-of-two tree)
      :FLOW-CONDITIONS (simplify-second-of-two tree)
      :IN nil
      :PROPERTY (simplify-second-of-two tree)
      :PROPERTY-CONDITION-OR-EXPRESSION (simplify-second-of-two tree)
      :OR nil
      :SPACE nil
      :STATE (list :PROPERTY-CONDITION
                   (list :SYMBOL "state")
                   '(:QUALIFIER
                     (:EQUIVALENCE
                      (:IS "is")))
                   (list :EXPRESSION
                         (list :VALUE (second tree))))
      :THEN nil
      :VALUE (simplify-second-of-two tree)
      (remove nil? (map simplify tree)))
    tree))

;; OK, there is a major unresolved problem. If there is a determiner condition,
;; the tree as parsed from natural language is the wrong shape, and we're 
;; going to have to restructure it somewhere to being the determiner upstream
;; of the property conditions. It *may* be possible to do that in `generate`.

(defn simplify-determiner-condition
  [tree]
  (apply vector
         (cons :DETERMINER-CONDITION
               (cons
                (simplify-second-of-two (second tree))
                (rest (rest tree))))))
