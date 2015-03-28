;; A very simple parser which parses production rules of the following forms:
;;
;; * "if altitude is less than 100 and state is forest then state should be climax and deer should be 3"
;; * "if altitude is 100 or fertility is 25 then state should be heath and fertility should be 24.3"
;; * "if altitude is 100 or fertility is 25 then state should be heath"
;; * "if deer is more than 2 and wolves is 0 and fertility is more than 20 then deer should be deer + 2"
;; * "if deer is more than 1 and wolves is more than 1 then deer should be deer - wolves"
;; * "if state is grassland and 4 neighbours have state equal to water then state should be village"
;; * "if state is forest and fertility is between 55 and 75 then state should be climax"
;; * "if 6 neighbours have state equal to water then state should be village"
;; * "if state is in grassland or pasture or heath and 4 neighbours are water then state should be village"
;; * "if state is forest or state is climax and some neighbours have state equal to fire then 3 in 5 chance that state should be fire"
;; * "if state is pasture and more than 3 neighbours have state equal to scrub then state should be scrub"
;; * 
;;
;; it generates rules in the form expected by `mw-engine.core`, q.v.
;;
;; It is, as I say, very simple; it generates a complete rule, or it fails completely, returning nil. 
;; Very occasionally it generates a wrong rule - one which is not a correct translation of the rule
;; semantics - but that is buggy behaviour, which I'll try to fix over the next few weeks, not a
;; design fault.
;;
;; More significantly it does not generate useful error messages on failure.
;;
;; This is the parser that is actually used currently; but see also insta.clj, 
;; which is potentially a much better parser but does not quite work yet.

(ns mw-parser.core
  (:use mw-engine.utils
        [clojure.string :only [split trim triml]])
  (:gen-class)
)

(declare parse-conditions)
(declare parse-not-condition)
(declare parse-simple-condition)

;; a regular expression which matches string representation of numbers
(def re-number #"^[0-9.]*$")

;; error thrown when an attempt is made to set a reserved property
(def reserved-properties-error 
  "The properties 'x' and 'y' of a cell are reserved and should not be set in rule actions")
;; error thrown when a rule cannot be parsed
(def bad-parse-error "I did not understand '%s'")

(defn- keyword-or-numeric
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
;; 1. A code fragment parsed from the front of the sequence of tokens, and
;; 2. the remaining tokens which were not consumed in constructing that fragment.
;;
;; In every case if the function cannot parse the desired construct from the
;; front of the sequence of tokens it returns nil.


(defn parse-numeric-value
  "Parse a number."
  [[value & remainder]]
  (if (and value (re-matches re-number value)) [(read-string value) remainder]))

(defn parse-property-int
  "Parse a token assumed to be the name of a property of the current cell, 
  whose value is assumed to be an integer."
  [[value & remainder]]
  (if value [(list 'get-int 'cell (keyword value)) remainder]))

(defn parse-property-value
  "Parse a token assumed to be the name of a property of the current cell."
  [[value & remainder]]
  (if value [(list (keyword value) 'cell) remainder]))

(defn parse-token-value
  "Parse a token assumed to be a simple token value."
  [[value & remainder]]
  (if value [(keyword value) remainder]))

(defn parse-simple-value
  "Parse a value from the first of these `tokens`. If `expect-int` is true, return
   an integer or something which will evaluate to an integer."
  ([tokens expect-int]
    (or
        (parse-numeric-value tokens)
        (cond expect-int
          (parse-property-int tokens)
          true (parse-token-value tokens))))
  ([tokens]
    (parse-simple-value tokens false)))

(defn gen-token-value
  "Parse a single value from this single token and return just the generated
   code, not a pair."
  [token expect-int]
  (first (parse-simple-value (list token) expect-int)))

(defn parse-disjunct-value
  "Parse a list of values from among these `tokens`. If `expect-int` is true, return
   integers or things which will evaluate to integers."
  [[OR token & tokens] expect-int]
  (cond (member? OR '("or" "in"))
    (let [value (first (parse-simple-value (list token) expect-int))
          seek-others (= (first tokens) "or")]
      (cond seek-others
        (let [[others remainder] (parse-disjunct-value tokens expect-int)]
          [(cons value others) remainder])
        true
        [(list value) tokens]))))
   
(defn parse-value 
  "Parse a value from among these `tokens`. If `expect-int` is true, return
   an integer or something which will evaluate to an integer."
  ([tokens expect-int]
    (or 
      (parse-disjunct-value tokens expect-int)
      (parse-simple-value tokens expect-int)))
  ([tokens]
    (parse-value tokens false)))

(defn parse-member-condition
  "Parses a condition of the form '[property] in [value] or [value]...'"
  [[property IS IN & rest]]
  (if (and (member? IS '("is" "are")) (= IN "in"))
    (let [[l remainder] (parse-disjunct-value (cons "in" rest) false)]
      [(list 'member? (list (keyword property) 'cell) (list 'quote l)) remainder])))

(defn- parse-less-condition
  "Parse '[property] less than [value]'."
  [[property IS LESS THAN & rest]]
  (cond (and (member? IS '("is" "are")) (member? LESS '("less" "fewer")) (= THAN "than"))
    (let [[value remainder] (parse-value rest true)]
        [(list '< (list 'get-int 'cell (keyword property)) value) remainder])))

(defn- parse-more-condition
  "Parse '[property] more than [value]'."
  [[property IS MORE THAN & rest]]
  (cond (and (member? IS '("is" "are")) (member? MORE '("more" "greater")) (= THAN "than"))
    (let [[value remainder] (parse-value rest true)]
        [(list '> (list 'get-int 'cell (keyword property)) value) remainder])))

(defn- parse-between-condition
  [[p IS BETWEEN v1 AND v2 & rest]]
  (cond (and (member? IS '("is" "are")) (= BETWEEN "between") (= AND "and") (not (nil? v2)))
    (let [property (first (parse-simple-value (list p) true))
          value1 (first (parse-simple-value (list v1) true))
          value2 (first (parse-simple-value (list v2) true))]
      [(list 'or
            (list '< value1 property value2)
            (list '> value1 property value2)) rest])))

(defn- parse-is-condition
  "Parse clauses of the form 'x is y', 'x is in y or z...', 
   'x is between y and z', 'x is more than y' or 'x is less than y'.
   It is necessary to disambiguate whether value is a numeric or keyword."
  [[property IS value & rest]]
  (cond 
    (member? IS '("is" "are"))
    (let [tokens (cons property (cons value rest))]
      (cond 
        (re-matches re-number value) [(list '= (list 'get-int 'cell (keyword property)) (read-string value)) rest]
        value [(list '= (list (keyword property) 'cell) (keyword value)) rest]))))

(defn- parse-not-condition 
  "Parse the negation of a simple condition."
  [[property IS NOT & rest]]
  (cond (and (member? IS '("is" "are")) (= NOT "not"))
    (let [partial (parse-simple-condition (cons property (cons "is" rest)))]
      (cond partial
        (let [[condition remainder] partial]
          [(list 'not condition) remainder])))))

(defn- gen-neighbours-condition
  ([comp1 quantity property value remainder comp2 distance] 
    [(list comp1  
         (list 'count
               (list 'get-neighbours-with-property-value 'world 
                     '(cell :x) '(cell :y) distance 
                     (keyword property) (keyword-or-numeric value) comp2))
         quantity)
           remainder])
  ([comp1 quantity property value remainder comp2]
    (gen-neighbours-condition comp1 quantity property value remainder comp2 1)))

(defn parse-comparator-neighbours-condition
  "Parse conditions of the form '...more than 6 neighbours are [condition]'"
  [[MORE THAN n NEIGHBOURS WITHIN distance have-or-are & rest]]
  (let [quantity (first (parse-numeric-value (list n)))
        comparator (cond (= MORE "more") '>
                     (member? MORE '("fewer" "less")) '<)]       
    (cond
      (not= WITHIN "within")
      (parse-comparator-neighbours-condition 
        (flatten 
          ;; two tokens were mis-parsed as 'within distance' that weren't
          ;; actually 'within' and a distance. Splice in 'within 1' and try
          ;; again.
          (list MORE THAN n NEIGHBOURS "within" "1" WITHIN distance have-or-are rest)))
      (and quantity 
           comparator
           (= THAN "than")
           (= NEIGHBOURS "neighbours"))
      (cond
        (= have-or-are "are") 
        (let [[value & remainder] rest
              dist (gen-token-value distance true)]
          (gen-neighbours-condition comparator quantity :state value remainder = dist))
        (= have-or-are "have")
        (let [[property comp1 comp2 value & remainder] rest
              dist (gen-token-value distance true)]
          (cond (and (= comp1 "equal") (= comp2 "to"))
            (gen-neighbours-condition comparator quantity property 
                                      value remainder = dist)
            (and (= comp1 "more") (= comp2 "than"))
            (gen-neighbours-condition comparator quantity property 
                                      value remainder > dist)
            (and (= comp1 "less") (= comp2 "than"))
            (gen-neighbours-condition comparator quantity property 
                                      value remainder < dist)
            ))))))
  
(defn parse-some-neighbours-condition
  [[SOME NEIGHBOURS & rest]]
  (cond
    (and (= SOME "some") (= NEIGHBOURS "neighbours"))
    (parse-comparator-neighbours-condition (concat '("more" "than" "0" "neighbours") rest))))

(defn parse-simple-neighbours-condition
  "Parse conditions of the form '...6 neighbours are [condition]'"
  [[n NEIGHBOURS WITHIN distance have-or-are & rest]]
  (let [quantity (first (parse-numeric-value (list n)))]       
    (cond
      (and quantity (= NEIGHBOURS "neighbours"))
      (cond
        (not= WITHIN "within")
        (parse-simple-neighbours-condition
          (flatten 
            ;; two tokens were mis-parsed as 'within distance' that weren't
            ;; actually 'within' and a distance. Splice in 'within 1' and try
            ;; again.
            (list n NEIGHBOURS "within" "1" WITHIN distance have-or-are rest)))
        (= have-or-are "are") 
        (let [[value & remainder] rest
              dist (gen-token-value distance true)]
          (gen-neighbours-condition '= quantity :state value remainder = dist))
        (= have-or-are "have")
        (let [[property comp1 comp2 value & remainder] rest
              dist (gen-token-value distance true)]
          (cond (and (= comp1 "equal") (= comp2 "to"))
            (gen-neighbours-condition '= quantity property value remainder = 
                                      dist)
            (and (= comp1 "more") (= comp2 "than"))
            (gen-neighbours-condition '= quantity property value remainder > 
                                      dist)
            (and (= comp1 "less") (= comp2 "than"))
            (gen-neighbours-condition '= quantity property value remainder < 
                                      dist)
            ))))))
  
(defn parse-neighbours-condition
  "Parse conditions referring to neighbours"
  [tokens]
  (or
    (parse-simple-neighbours-condition tokens)
    (parse-comparator-neighbours-condition tokens)
    (parse-some-neighbours-condition tokens)
    ))

(defn parse-simple-condition
  "Parse conditions of the form '[property] [comparison] [value]'."
  [tokens]
  (or
    (parse-neighbours-condition tokens)
    (parse-member-condition tokens)
    (parse-not-condition tokens)
    (parse-less-condition tokens)
    (parse-more-condition tokens)
    (parse-between-condition tokens)
    (parse-is-condition tokens)))

(defn- parse-disjunction-condition
  "Parse '... or [condition]' from `tokens`, where `left` is the already parsed first disjunct."
  [left tokens]
  (let [partial (parse-conditions tokens)]
    (if partial
      (let [[right remainder] partial]
        [(list 'or left right) remainder]))))

(defn- parse-conjunction-condition
  "Parse '... and [condition]' from `tokens`, where `left` is the already parsed first conjunct."
  [left tokens]
  (let [partial (parse-conditions tokens)]
    (if partial
      (let [[right remainder] partial]
        [(list 'and left right) remainder]))))

(defn- parse-conditions
  "Parse conditions from `tokens`, where conditions may be linked by either 'and' or 'or'."
  [tokens]
  (let [partial (parse-simple-condition tokens)]
    (if partial
      (let [[left [next & remainder]] partial]
        (cond
          (= next "and") (parse-conjunction-condition left remainder)
          (= next "or") (parse-disjunction-condition left remainder)
          true partial)))))

(defn- parse-left-hand-side
 "Parse the left hand side ('if...') of a production rule."
 [[IF & tokens]]
 (if
   (= IF "if")
   (parse-conditions tokens)))

(defn- parse-arithmetic-action 
  "Parse actions of the form '[property] should be [property] [arithmetic-operator] [value]',
   e.g. 'fertility should be fertility + 1', or 'deer should be deer - wolves'."
  [previous [prop1 SHOULD BE prop2 operator value & rest]]
  (cond
    (member? prop1 '("x" "y"))
    (throw 
      (Exception. reserved-properties-error))
    (and (= SHOULD "should")
           (= BE "be")
           (member? operator '("+" "-" "*" "/")))
    [(list 'merge (or previous 'cell)
           {(keyword prop1) (list 'int 
                                  (list (symbol operator) (list 'get-int 'cell (keyword prop2))
                                        (cond
                                          (re-matches re-number value) (read-string value)
                                          true (list 'get-int 'cell (keyword value)))))}) rest]))

(defn- parse-set-action 
  "Parse actions of the form '[property] should be [value].'"
  [previous [property SHOULD BE value & rest]]
  (cond 
    (member? property '("x" "y"))
    (throw 
      (Exception. reserved-properties-error))
    (and (= SHOULD "should") (= BE "be"))
    [(list 'merge (or previous 'cell)
           {(keyword property) (cond (re-matches re-number value) (read-string value) true (keyword value))}) rest]))

(defn- parse-simple-action [previous tokens]
  (or (parse-arithmetic-action previous tokens)
      (parse-set-action previous tokens)))

(defn- parse-actions
  "Parse actions from tokens."
  [previous tokens]
  (let [[left remainder] (parse-simple-action previous tokens)]
    (cond left
          (cond (= (first remainder) "and")
                (parse-actions left (rest remainder))
                true (list left)))))

(defn- parse-probability 
  "Parse a probability of an action from this collection of tokens"
  [previous [n CHANCE IN m & tokens]]
  (cond 
    (and (= CHANCE "chance")(= IN "in"))
    (let [[action remainder] (parse-actions previous tokens)]
      (cond action
        [(list 'cond 
              (list '< 
                    (list 'rand 
                          (first (parse-simple-value (list m) true)))
                    (first (parse-simple-value (list n) true))) 
              action) remainder])))) 

(defn- parse-right-hand-side
  "Parse the right hand side ('then...') of a production rule."
  [[THEN & tokens]]
  (if (= THEN "then")
    (or
      (parse-probability nil tokens)
      (parse-actions nil tokens))))

(defn parse-rule 
  "Parse a complete rule from this `line`, expected to be either a string or a 
   sequence of string tokens. Return the rule in the form of an S-expression.

   Throws an exception if parsing fails."
  [line]
  (cond
   (string? line) 
   (let [rule (parse-rule (split (triml line) #"\s+"))]
     (cond rule rule
       true (throw (Exception. (format bad-parse-error line)))))
   true 
   (let [[left remainder] (parse-left-hand-side line)
              [right junk] (parse-right-hand-side remainder)]
     (cond 
       ;; there should be a valide left hand side and a valid right hand side
       ;; there shouldn't be anything left over (junk should be empty)
       (and left right (empty? junk))
       (list 'fn ['cell 'world] (list 'if left right))))))

(defn compile-rule 
  "Parse this `rule-text`, a string conforming to the grammar of MicroWorld rules,
   into Clojure source, and then compile it into an anonymous
   function object, getting round the problem of binding mw-engine.utils in
   the compiling environment. If `return-tuple?` is present and true, return
   a list comprising the anonymous function compiled, and the function from
   which it was compiled.

   Throws an exception if parsing fails."
  ([rule-text return-tuple?]
    (do
      (use 'mw-engine.utils)
      (let [afn (eval (parse-rule rule-text))]
        (cond 
          (and afn return-tuple?)(list afn (trim rule-text))
          true afn))))
  ([rule-text]
    (compile-rule rule-text false)))
