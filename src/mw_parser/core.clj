;; A very simple parser which parses production rules of the following forms:
;;
;; * "if altitude is less than 100 and state is forest then state should be climax and deer should be 3"
;; * "if altitude is 100 or fertility is 25 then state should be heath and fertility should be 24.3"
;; * "if altitude is 100 or fertility is 25 then state should be heath"
;; * "if deer is more than 2 and wolves is 0 and fertility is more than 20 then deer should be deer + 2"
;; * "if deer is more than 1 and wolves is more than 1 then deer should be deer - wolves"
;; * "if state is grassland and 4 neighbours have state equal to water then state should be village"
;; * "if state is forest and fertility is between 55 and 75 then state should be climax"
;; * "if 6 neighbours have state equal to water then state should be fishery"
;;
;; It should also but does not yet parse rules of the form:

;; * "if state is forest or state is climax and some neighbours have state is fire then 3 in 5 chance that state should be fire"
;; * "if state is pasture and more than 3 neighbours have state equal to scrub then state should be scrub"
;; * "if state is in grassland or pasture or heath and 4 neighbours are water then state should be village"
;;
;; it generates rules in the form expected by mw-engine.core

(ns mw-parser.core
  (:use mw-engine.utils
        [clojure.string :only [split triml]]))

(declare parse-conditions)
(declare parse-not-condition)
(declare parse-simple-condition)

;; a regular expression which matches string representation of numbers
(def re-number #"^[0-9.]*$")

(defn keyword-or-numeric
  "If this token appears to represent an explicit number, return that number;
   otherwise, make a keyword of it and return that."
  [token]
  (cond 
    (re-matches re-number token) (read-string token)
    (keyword? token) token
    true (keyword token)))

;; Generally all functions in this file with names beginning 'parse-' take a 
;; sequence of tokens (and in some cases other optional arguments) and return a
;; vector comprising
;;
;; # A code fragment parsed from the front of the sequence of tokens, and
;; # the remaining tokens which were not consumed in constructing that sequence.
;;
;; In every case if the function cannot parse the desired construct from the
;; front of the sequence of tokens it returns nil.

(defn parse-numeric-value
  "Parse a number."
  [[value & remainder]]
  (if (re-matches re-number value) [(read-string value) remainder]))

(defn parse-property-int
  "Parse a token assumed to be the name of a property of the current cell, 
  whose value is assumed to be an integer."
  [[value & remainder]]
  (if value [(list 'get-int 'cell (keyword value)) remainder]))

(defn parse-property-value
  "Parse a token assumed to be the name of a property of the current cell."
  [[value & remainder]]
  (if value [(list (keyword value) 'cell) remainder]))

(defn parse-simple-value
  "Parse a value from the first of these `tokens`. If `expect-int` is true, return
   an integer or something which will evaluate to an integer."
  ([tokens expect-int]
    (or
        (parse-numeric-value tokens)
        (cond expect-int
          (parse-property-int tokens)
          true (parse-property-value tokens))))
  ([tokens]
    (parse-simple-value tokens false)))

(defn parse-disjunct-value
  "Parse a list of values from among these `tokens`. If `expect-int` is true, return
   an integer or something which will evaluate to an integer."
  [[OR token & tokens] expect-int]
  (println OR)
  (cond (member? OR '("or" "in"))
    (let [[others remainder] (parse-disjunct-value2 tokens expect-int)]
      [(cons 
         (cond 
           expect-int (first (parse-simple-value (list token) true))
           true (keyword token)) 
         others) 
       remainder])
    true [nil (cons OR (cons token tokens))]))
   
(defn parse-value 
  "Parse a value from among these `tokens`. If `expect-int` is true, return
   an integer or something which will evaluate to an integer."
  ([tokens expect-int]
    (or 
      (parse-disjunct-value tokens expect-int)
      (parse-simple-value tokens)))
  ([tokens]
    (parse-value tokens false)))

(defn parse-member-condition
  [[property IN & rest]]
  (if (= IN "in")
    (let [[l remainder] (parse-disjunct-value (cons "in" rest) false)]
      [(list 'member? (keyword property) l) remainder])))

(defn parse-less-condition
  "Parse '[property] less than [value]'."
  [[property LESS THAN value & rest]]
  (cond (and (= LESS "less") (= THAN "than"))
        [(list '< (list 'get-int 'cell (keyword property)) (read-string value)) rest]))

(defn parse-more-condition
  "Parse '[property] more than [value]'."
  [[property MORE THAN value & rest]]
  (cond (and (= MORE "more") (= THAN "than"))
        [(list '> (list 'get-int 'cell (keyword property)) (read-string value)) rest]))

(defn parse-between-condition
  [[p BETWEEN v1 AND v2 & rest]]
  (cond (and (= BETWEEN "between") (= AND "and") (not (nil? v2)))
    (let [property (first (parse-simple-value (list p) true))
          value1 (first (parse-simple-value (list v1) true))
          value2 (first (parse-simple-value (list v2) true))]
      [(list 'or
            (list '< value1 property value2)
            (list '> value1 property value2)) rest])))

(defn parse-is-condition
  "Parse clauses of the form 'x is y', 'x is in y or z...', 
   'x is between y and z', 'x is more than y' or 'x is less than y'.
   It is necessary to disambiguate whether value is a numeric or keyword."
  [[property IS value & rest]]
  (cond 
    (member? IS '("is" "are"))
    (let [tokens (cons property (cons value rest))]
      (cond 
        (= value "in") (parse-member-condition tokens)
        (= value "between") (parse-between-condition tokens)
        (= value "more") (parse-more-condition tokens)
        (= value "less") (parse-less-condition tokens)
        (re-matches re-number value) [(list '= (list 'get-int 'cell (keyword property)) (read-string value)) rest]
        value [(list '= (list (keyword property) 'cell) (keyword value)) rest]))))

(defn parse-not-condition 
  "Parse the negation of a simple condition."
  [[property IS NOT & rest]]
  (cond (and (member? IS '("is" "are")) (= NOT "not"))
    (let [partial (parse-simple-condition (cons property (cons "is" rest)))]
      (cond partial
        (let [[condition remainder] partial]
          [(list 'not condition) remainder])))))

(defn gen-neighbours-condition
  [comparator quantity property value remainder] 
  [(list comparator quantity 
         (list 'count
               (list 'get-neighbours-with-property-value 'world 'cell 
                     (keyword property) (keyword-or-numeric value))))
           remainder])

(defn parse-simple-neighbours-condition
  [[n NEIGHBOURS have-or-are & rest]]
  (let [quantity (first (parse-numeric-value (list n)))]       
    (cond
      (and quantity (= NEIGHBOURS "neighbours"))
      (cond
        (= have-or-are "are") 
        (let [[value & remainder] rest]
          (gen-neighbours-condition '= quantity :state value remainder))
        (= have-or-are "have")
        (let [[property EQUAL TO value & remainder] rest]
          (cond (and (= EQUAL "equal") (= TO "to"))
            (gen-neighbours-condition '= quantity property value remainder)))))))
  
(defn parse-neighbours-condition
  "Parse conditions referring to neighbours"
  [tokens]
  (or
    (parse-simple-neighbours-condition tokens)
;;    (parse-more-than-neighbours-condition tokens)
;;    (parse-fewer-than-neighbours-condition tokens)
    ))

(defn parse-simple-condition
  "Parse conditions of the form '[property] [comparison] [value]'."
  [tokens]
  (or
    (parse-simple-neighbours-condition tokens)
    (parse-member-condition tokens)
    (parse-not-condition tokens)
    (parse-is-condition tokens)
    (parse-less-condition tokens)
    (parse-more-condition tokens)))

(defn parse-disjunction-condition
  "Parse '... or [condition]' from `tokens`, where `left` is the already parsed first disjunct."
  [left tokens]
  (let [partial (parse-conditions tokens)]
    (if partial
      (let [[right remainder] partial]
        [(list 'or left right) remainder]))))

(defn parse-conjunction-condition
  "Parse '... and [condition]' from `tokens`, where `left` is the already parsed first conjunct."
  [left tokens]
  (let [partial (parse-conditions tokens)]
    (if partial
      (let [[right remainder] partial]
        [(list 'and left right) remainder]))))

(defn parse-conditions
  "Parse conditions from `tokens`, where conditions may be linked by either 'and' or 'or'."
  [tokens]
  (let [partial (parse-simple-condition tokens)]
    (if partial
      (let [[left [next & remainder]] partial]
        (cond
          (= next "and") (parse-conjunction-condition left remainder)
          (= next "or") (parse-disjunction-condition left remainder)
          true partial)))))

(defn parse-left-hand-side
 "Parse the left hand side ('if...') of a production rule."
 [tokens]
 (if
   (= (first tokens) "if")
   (parse-conditions (rest tokens))))

(defn parse-arithmetic-action 
  "Parse actions of the form '[property] should be [property] [arithmetic-operator] [value]',
   e.g. 'fertility should be fertility + 1', or 'deer should be deer - wolves'."
  [previous [prop1 should be prop2 operator value & rest]]
  (if (and (= should "should")
           (= be "be")
           (member? operator '("+" "-" "*" "/")))
    [(list 'merge (or previous 'cell)
           {(keyword prop1) (list (symbol operator) (list 'get-int 'cell (keyword prop2))
                                  (cond
                                    (re-matches re-number value) (read-string value)
                                    true (list 'get-int 'cell (keyword value))))}) rest]))

(defn parse-set-action 
  "Parse actions of the form '[property] should be [value].'"
  [previous [property should be value & rest]]
  (if (and (= should "should") (= be "be"))
    [(list 'merge (or previous 'cell)
           {(keyword property) (cond (re-matches re-number value) (read-string value) true (keyword value))}) rest]))

(defn parse-simple-action [previous tokens]
  (or (parse-arithmetic-action previous tokens)
      (parse-set-action previous tokens)))

(defn parse-actions
  "Parse actions from tokens."
  [previous tokens]
  (let [[left remainder] (parse-simple-action previous tokens)]
    (cond left
          (cond (= (first remainder) "and")
                (parse-actions left (rest remainder))
                true (list left)))))

(defn parse-right-hand-side
  "Parse the right hand side ('then...') of a production rule."
  [tokens]
  (if (= (first tokens) "then")
    (parse-actions nil (rest tokens))))

(defn parse-rule 
  "Parse a complete rule from this string or sequence of string tokens."
  [line]
  (cond
   (string? line) (parse-rule (split (triml line) #"\s+"))
   true (let [[left remainder] (parse-left-hand-side line)
              [right junk] (parse-right-hand-side remainder)]
          ;; there shouldn't be any junk (should be null)
          (list 'fn ['cell 'world] (list 'if left right))
          )))

