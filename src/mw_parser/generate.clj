(ns ^{:doc "Generate Clojure source from simplified parse trees."
      :author "Simon Brooke"}
 mw-parser.generate 
  (:require 
   [mw-engine.utils :refer :all] ;; may need these when macro-expanding rules.
   [mw-parser.utils :refer [assert-type search-tree TODO]]))

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

(declare generate generate-action)

(def reserved-properties-error
  "The properties 'x' and 'y' of a cell are reserved and should not be set in rule actions")

;;; macros used in generated rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; production (if-then) rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generate-rule
  "From this `tree`, assumed to be a syntactically correct rule specification,
  generate and return the appropriate rule as a function of two arguments."
  [tree]
  (assert-type tree :RULE)
  (vary-meta
   ;; do macro-expansion here, because at least in theory I know what
   ;; macros are in scope here.
   (macroexpand
    (list 'fn ['cell 'world] (list 'when (generate (nth tree 2)) (generate (nth tree 3)))))
   merge
   {:rule-type
    :production}))

(defn generate-conditions
  "From this `tree`, assumed to be a syntactically correct conditions clause,
  generate and return the appropriate clojure fragment."
  [tree]
  (assert-type tree :CONDITIONS)
  (generate (second tree)))

(defn generate-condition
  "From this `tree`, assumed to be a syntactically correct condition clause,
  generate and return the appropriate clojure fragment."
  [tree]
  (assert-type tree :CONDITION)
  (generate (second tree)))

(defn generate-conjunct-condition
  "From this `tree`, assumed to be a syntactically conjunct correct condition clause,
  generate and return the appropriate clojure fragment."
  [tree]
  (assert-type tree :CONJUNCT-CONDITION)
  (cons 'and (map generate (rest tree))))

(defn generate-disjunct-condition
  "From this `tree`, assumed to be a syntactically correct disjunct condition clause,
  generate and return the appropriate clojure fragment."
  [tree]
  (assert-type tree :DISJUNCT-CONDITION)
  (cons 'or (map generate (rest tree))))

(defn generate-ranged-property-condition
  "From this `tree`, assumed to be a syntactically property condition clause for
  this `property` where the `expression` is a numeric range, generate and return
  the appropriate clojure fragment."
  [tree property expression]
  (assert-type tree :PROPERTY-CONDITION)
  (assert-type (nth tree 3) :RANGE-EXPRESSION)
  (let [l1 (generate (nth expression 2))
        l2 (generate (nth expression 4))
        pv (list property 'cell)]
    (list 'let ['lower (list 'min l1 l2)
                'upper (list 'max l1 l2)]
          (list 'and (list '>= pv 'lower) (list '<= pv 'upper)))))

(defn generate-disjunct-property-condition
  "From this `tree`, assumed to be a syntactically property condition clause
  where the expression is a a disjunction, generate and return
  the appropriate clojure fragment.
  TODO: this is definitely still wrong!"
  ([tree]
   (let [property (generate (second tree))
         qualifier (generate (nth tree 2))
         expression (generate (nth tree 3))]
     (generate-disjunct-property-condition tree property qualifier expression)))
  ([_tree property qualifier expression]
   (let [e (list expression (list property 'cell))]
     (if (= qualifier '=) e
         (list 'not e)))))

(defn generate-property-condition
  "From this `tree`, assumed to be a syntactically property condition clause,
  generate and return the appropriate clojure fragment."
  ([tree]
   (assert-type tree :PROPERTY-CONDITION)
   (if
    (and (= (count tree) 2) (= (first (second tree)) :SYMBOL))
     ;; it's a shorthand for 'state equal to symbol'. This should probably have
     ;; been handled in simplify...
     (generate-property-condition
      (list
       :PROPERTY-CONDITION
       '(:SYMBOL "state")
       '(:QUALIFIER (:EQUIVALENCE (:EQUAL "equal to")))
       (second tree)))
     ;; otherwise...
     (generate-property-condition tree (first (nth tree 3)))))
  ([tree expression-type]
   (assert-type tree :PROPERTY-CONDITION)
   (let [property (generate (second tree))
         qualifier (generate (nth tree 2))
         e (generate (nth tree 3))
         expression (cond
                      (and (not (= qualifier '=)) (keyword? e)) (list 'or (list e 'cell) e)
                      (and (not (= qualifier 'not=)) (keyword? e)) (list 'or (list e 'cell) e)
                      :else e)]
     (case expression-type
       :DISJUNCT-EXPRESSION (generate-disjunct-property-condition tree property qualifier expression)
       :RANGE-EXPRESSION (generate-ranged-property-condition tree property expression)
       (list qualifier (if (number? expression)
                         (list 'mw-engine.utils/get-num 'cell property)
                         (list property 'cell)) expression)))))

(defn generate-qualifier
  "From this `tree`, assumed to be a syntactically correct qualifier,
  generate and return the appropriate clojure fragment."
  [tree]
  (if
   (= (count tree) 2)
    (generate (second tree))
    ;; else
    (generate (nth tree 2))))

(defn generate-simple-action
  "From this `tree`, assumed to be a syntactically correct simple action,
  generate and return the appropriate clojure fragment."
  ([tree]
   (assert-type tree :SIMPLE-ACTION)
   (generate-simple-action tree []))
  ([tree others]
   (assert-type tree :SIMPLE-ACTION)
   (let [property (generate (second tree))
         expression (generate (nth tree 3))]
     (if (or (= property :x) (= property :y))
       (throw (Exception. reserved-properties-error))
       (list 'merge
             (if (empty? others) 'cell
               ;; else
                 (generate others))
             {property expression})))))

(defn trap-errors-in-dice-throw
  "We're getting a wierd -- many would say 'impossible' -- intermittent bug
   which appears to happen here. "
  [sides chances action]
  ;; (list 'try 
        (list 'if (list '< (list 'rand sides) chances) action)
        ;; (list 'catch 'Exception 'any
        ;;       (list 'println (list 'format "Dice throw bug %d/%d" chances sides))
        ;;       (list 'throw (list 'ex-info "Error in dice throw"
        ;;                          {:total sides
        ;;                           :chances chances
        ;;                           :action action}
        ;;                          'any))))
  )

(defn generate-probable-action
  "From this `tree`, assumed to be a syntactically correct probable action,
  generate and return the appropriate clojure fragment."
  ([tree]
   (assert-type tree :PROBABLE-ACTION)
   (generate-probable-action tree []))
  ([tree others]
   (assert-type tree :PROBABLE-ACTION)
   (let
    [chances (generate (nth tree 1))
     total (generate (nth tree 2))
     action (generate-action (nth tree 3) others)]
    ;; TODO: could almost certainly be done better with macro syntax
     (trap-errors-in-dice-throw total chances action))))

(defn generate-action
  "From this `tree`, assumed to be a syntactically correct action,
  generate and return the appropriate clojure fragment."
  [tree others]
  (case (first tree)
    :ACTIONS (generate-action (first tree) others)
    :SIMPLE-ACTION (generate-simple-action tree others)
    :PROBABLE-ACTION (generate-probable-action tree others)
    (throw (Exception. (str "Not a known action type: " (first tree))))))

(defn generate-multiple-actions
  "From this `tree`, assumed to be one or more syntactically correct actions,
  generate and return the appropriate clojure fragment."
  [tree]
  (assert-type tree :ACTIONS)
  (generate-action (first (rest tree)) (second (rest tree))))

(defn generate-numeric-expression
  "From this `tree`, assumed to be a syntactically correct numeric expression,
  generate and return the appropriate clojure fragment."
  [tree]
  (assert-type tree :NUMERIC-EXPRESSION)
  (case (count tree)
    4 (let [[p operator expression] (rest tree)
            property (if (number? p) p (list 'mw-engine.utils/get-num 'cell p))]
        (list (generate operator) (generate property) (generate expression)))
    (case (first (second tree))
      :SYMBOL (list 'mw-engine.utils/get-num 'cell (generate (second tree)))
      (generate (second tree)))))

(defn generate-neighbours-condition
  "Generate code for a condition which refers to neighbours."
  ([tree]
   (assert-type tree :NEIGHBOURS-CONDITION)
   (case (first (second tree))
     :NUMBER (read-string (second (second tree)))
     :QUANTIFIER (generate-neighbours-condition tree (first (second (second tree))))
     :QUALIFIER (cons (generate (second tree)) (rest (generate (nth tree 2))))))
  ([tree quantifier-type]
   (let [quantifier (second tree)
         pc (generate (nth tree 4))]
     (case quantifier-type
       :NUMBER (generate-neighbours-condition '= (read-string (second (second quantifier))) pc 1)
       :SOME (generate-neighbours-condition '> 0 pc 1)
       :MORE (let [value (generate (nth quantifier 3))]
               (generate-neighbours-condition '> value pc 1))
       :LESS (let [value (generate (nth quantifier 3))]
               (generate-neighbours-condition '< value pc 1)))))
  ([comp1 quantity property-condition distance]
   (list comp1
         (list 'count
               (list 'remove 'false?
                     (list 'map (list 'fn ['cell] property-condition)
                           (list 'mw-engine.utils/get-neighbours 'world 'cell distance))))
         quantity))
  ([comp1 quantity property-condition]
   (generate-neighbours-condition comp1 quantity property-condition 1)))

(defn generate-within-condition
  "Generate code for a condition which refers to neighbours within a specified distance.
  NOTE THAT there's clearly masses of commonality between this and
  `generate-neighbours-condition`, and that some refactoring is almost certainly
  desirable. It may be that it's better to simplify a `NEIGHBOURS-CONDITION`
  into a `WITHIN-CONDITION` in the simplification stage."
  ([tree]
   (assert-type tree :WITHIN-CONDITION)
   (case (first (second tree))
     :QUANTIFIER (generate-within-condition tree (first (second (second tree))))
     :QUALIFIER (TODO "qualified within... help!")))
  ([tree quantifier-type]
   (let [quantifier (second tree)
         distance (generate (nth tree 4))
         pc (generate (nth tree 6))]
     (case quantifier-type
       :NUMBER (generate-neighbours-condition
                '=
                (read-string (second (second quantifier)))
                pc
                distance)
       :SOME (generate-neighbours-condition '> 0 pc distance)
       :MORE (let [value (generate (nth quantifier 3))]
               (generate-neighbours-condition '> value pc distance))
       :LESS (let [value (generate (nth quantifier 3))]
               (generate-neighbours-condition '< value pc distance))))))

;;; Flow rules. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A flow rule DOES NOT return a modified cell; instead, it 
;;; returns a PLAN to modify the world, in the form of a sequence of `flow`
;;; objects. See `mw-engine.flow`
;;;
;;; It is only when the plan is executed that the world is modified.

(defn flow-rule
  "Generate a flow rule for this `quantity` of this `property` from this 
   `source` to this `destination`.
   
   A flow rule **does not** return a modified cell; instead, it 
   returns a **plan** to modify the world, in the form of a sequence of 
   `flow` objects. See `mw-engine.flow`

   It is only when the plan is executed that the world is modified."
  [source property quantity-frag destinations]
  (vary-meta
   ;; do macro-expansion here, because at least in theory I know what
   ;; macros are in scope here.
   (macroexpand
    (list 'fn ['cell 'world]
          (list 'when 
                (list 'and 
                      source 
                      (list 'pos? 
                            (list 'mw-engine.utils/get-num 'cell property)))
                (list 'map
                      (list 'fn ['d]
                            {:source (list 'select-keys 'cell [:x :y])
                             :destination (list 'select-keys 'd [:x :y])
                             :property property
                             :quantity quantity-frag})
                      destinations))))
   merge
   {:rule-type
    :flow}))

(defn generate-quantity-accessor
  "Generate a code fragment which will generate the appropriate quantity of
   the `property` specified in a rule, from this `q-clause`."
  [q-clause property]
  (case (first q-clause)
    ;; TODO :EXPRESSION still needed
    :NUMBER (generate q-clause)
    :PERCENTAGE (let [multiplier (/ (generate (second q-clause)) 100)]
                  (list '* multiplier 
                        (list 'mw-engine.utils/get-num 'cell property)))
    :SIMPLE-EXPRESSION (if (= (count q-clause) 2)
                         (generate-quantity-accessor (second q-clause) 
                                                     property)
                         (throw (ex-info
                                 (format 
                                  "Cannot yet handle q-clause of form: `%s`" 
                                  q-clause)
                                 {:clause q-clause
                                  :property property})))
    :SOME (list 'rand (list 'mw-engine.utils/get-num 'cell property))
    (throw (ex-info
            (format "Unexpected QUANTITY type: `%s`" (first q-clause))
            {:clause q-clause
             :property property}))))

(defn generate-target-state-filter
  [clause targets-frag]
  (assert-type clause :DESTINATION)
  (list 'filter
        (list 'fn ['cell]
              (generate-property-condition
               (search-tree (search-tree clause :TARGET)
                            :PROPERTY-CONDITION)))
        targets-frag))

(defn generate-dest-accessor
  [clause]
  (let [dc (search-tree clause :DETERMINER-CONDITION)
        range (search-tree clause :RANGE)
        distance (if range (generate (nth range 2)) 1)]
    (list 'let ['candidates
                (generate-target-state-filter
                 clause
                 (list 'mw-engine.utils/get-neighbours 
                       'world 'cell distance))]
          (if dc
            (list 'list
                  (let [determiner (first 
                                    (second 
                                     (search-tree dc :DETERMINER)))
                        prop (generate (nth dc 2))]
                    (case determiner
                      :LEAST (list 'mw-engine.utils/get-least-cell 
                                   'candidates prop)
                      :MOST (list 'mw-engine.utils/get-most-cell 
                                  'candidates prop))))
            'candidates))))

(defn generate-flow
  [tree]
  (assert-type tree :FLOW-RULE)
  (let [clauses (reduce #(assoc %1 (first %2) %2) {} (rest tree))
        source-accessor (generate (:SOURCE clauses))
        property (generate (:SYMBOL clauses))
        quantity (generate-quantity-accessor (second (:QUANTITY clauses)) property)
        dest-accessor (generate-dest-accessor (:DESTINATION clauses))]
    (flow-rule source-accessor property quantity dest-accessor)))

;;; Top level; only function anything outside this file (except tests) should 
;;; really call.

(defn generate
  "Generate code for this (fragment of a) parse tree"
  [tree]
  (if
   (coll? tree)
    (case (first tree)
      :ACTIONS (generate-multiple-actions tree)
      :COMPARATIVE (generate (second tree))
      :COMPARATIVE-QUALIFIER (generate (second tree))
      :CONDITION (generate-condition tree)
      :CONDITIONS (generate-conditions tree)
      :CONJUNCT-CONDITION (generate-conjunct-condition tree)
      :DISJUNCT-CONDITION (generate-disjunct-condition tree)
      :DISJUNCT-EXPRESSION (set (generate (second tree)))
      :DISJUNCT-VALUE (map generate (rest tree))
      :EQUIVALENCE '=
      :EXPRESSION (generate (second tree))
      :FLOW-RULE (generate-flow tree)
      :LESS '<
      :MORE '>
      :NEGATED-QUALIFIER (case (generate (second tree))
                           = 'not=
                           > '<
                           < '>)
      :NEIGHBOURS-CONDITION (generate-neighbours-condition tree)
      :NUMERIC-EXPRESSION (generate-numeric-expression tree)
      :NUMBER (read-string (second tree))
      :OPERATOR (symbol (second tree))
      :PROBABLE-ACTION (generate-probable-action tree)
      :PROPERTY (list (generate (second tree)) 'cell) ;; dubious - may not be right
      :PROPERTY-CONDITION (generate-property-condition tree)
      :QUALIFIER (generate-qualifier tree)
      :QUANTITY (generate (second tree))
      :RULE (generate-rule tree)
      :SIMPLE-ACTION (generate-simple-action tree)
      :SOURCE (generate (second tree))
      :SYMBOL (keyword (second tree))
      :VALUE (generate (second tree))
      :WITHIN-CONDITION (generate-within-condition tree)
      (map generate tree))
    tree))
