(ns mw-parser.simplify
  (:use mw-engine.utils
        mw-parser.utils))

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
  (if (= (count tree) 2) (simplify (second tree)) tree))


(defn simplify
  "Simplify/canonicalise this `tree`. Opportunistically replace complex fragments with
  semantically identical simpler fragments"
  [tree]
  (if
    (coll? tree)
    (case (first tree)
      :ACTION (simplify-second-of-two tree)
      :ACTIONS (cons (first tree) (simplify (rest tree)))
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
      (remove nil? (map simplify tree)))
    tree))
