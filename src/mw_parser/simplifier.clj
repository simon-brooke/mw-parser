(ns mw-parser.simplifier
  (:use mw-engine.utils
        mw-parser.parser))

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


(defn simplify-some
  "'some' is the same as 'more than zero'"
  [tree]
  [:COMPARATIVE '> 0])

(defn simplify-none
  "'none' is the same as 'zero'"
  [tree]
  [:COMPARATIVE '= 0])

(defn simplify-all
  "'all' isn't actually the same as 'eight', because cells at the edges of the world have
  fewer than eight neighbours; but it's a simplifying (ha!) assumption for now."
  [tree]
  [:COMPARATIVE '= 8])

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
      :SPACE nil
      :QUALIFIER (simplify-qualifier tree)
      :CONDITIONS (simplify-second-of-two tree)
      :CONDITION (simplify-second-of-two tree)
      :EXPRESSION (simplify-second-of-two tree)
      :COMPARATIVE (simplify-second-of-two tree)
      :QUANTIFIER (simplify-quantifier tree)
      :VALUE (simplify-second-of-two tree)
      :PROPERTY (simplify-second-of-two tree)
      :ACTIONS (simplify-second-of-two tree)
      :ACTION (simplify-second-of-two tree)
      :ALL (simplify-all tree)
      :SOME (simplify-some tree)
      :NONE (simplify-none tree)
      (remove nil? (map simplify tree)))
    tree))

(simplify (parse-rule "if state is climax and 4 neighbours have state equal to fire then 3 chance in 5 state should be fire"))
(simplify (parse-rule "if state is climax and no neighbours have state equal to fire then 3 chance in 5 state should be fire"))

(simplify (parse-rule "if state is in grassland or pasture or heath and more than 4 neighbours have state equal to water then state should be village"))

(simplify (parse-rule "if 6 neighbours have state equal to water then state should be village"))

(simplify (parse-rule "if fertility is between 55 and 75 then state should be climax"))

(simplify (parse-rule "if state is forest then state should be climax"))


(simplify (parse-rule "if state is in grassland or pasture or heath and more than 4 neighbours have state equal to water then state should be village"))
(simplify (parse-rule "if altitude is less than 100 and state is forest then state should be climax and deer should be 3"))
(simplify (parse-rule "if altitude is 100 or fertility is 25 then state should be heath and fertility should be 24.3"))
(simplify (parse-rule "if altitude is 100 or fertility is 25 then state should be heath"))

(simplify (parse-rule "if deer is more than 2 and wolves is 0 and fertility is more than 20 then deer should be deer + 2"))
(simplify (parse-rule "if deer is more than 1 and wolves is more than 1 then deer should be deer - wolves"))
(simplify (parse-rule "if state is grassland and 4 neighbours have state equal to water then state should be village"))
