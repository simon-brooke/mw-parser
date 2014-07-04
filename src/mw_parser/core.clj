;; A very simple parser which parses production rules of the following forms:
;;
;; "if altitude is less than 100 and state is forest then state should be climax and deer should be 3"
;; "if altitude is 100 or fertility is 25 then state should be heath and fertility should be 24.3"
;; "if altitude is 100 or fertility is 25 then state should be heath"
;; "if deer is more than 2 and wolves is 0 and fertility is more than 20 then deer should be deer + 2"
;; "if deer is more than 1 and wolves is more than 1 then deer should be deer - wolves"
;;
;; It should also but does not yet parse rules of the form
;; "if 6 neighbours have state is water then state should be fishery"
;; "if state is forest or state is climax and some neighbours have state is fire then 3 in 5 chance that state should be fire"
;; "if state is pasture and more than 3 neighbours have state is scrub then state should be scrub"
;;
;; it generates rules in the form expected by mw-engine.core

(ns mw-parser.core
  (:use mw-engine.utils
        [clojure.string :only [split triml]]))

(declare parse-conditions)
(declare parse-not-condition)
(declare parse-simple-condition)

(defn parse-less-condition
  "Parse '[property] is less than [value]."
  [[property is less than value & rest]]
  (cond (and (member? is '("is" "are")) (= less "less") (= than "than"))
        [(list '< (list 'get-int 'cell (keyword property)) (read-string value)) rest]))

(defn parse-more-condition
  "Parse '[property] is more than [value]."
  [[property is more than value & rest]]
  (cond (and (member? is '("is" "are")) (= more "more") (= than "than"))
        [(list '> (list 'get-int 'cell (keyword property)) (read-string value)) rest]))

(defn parse-is-condition
  "Parse clauses of the form 'x is y', but not 'x is more than y' or 'x is less than y'.
   It is necessary to disambiguate whether value is a numeric or keyword."
  [[property is value & rest]]
  (cond (and (member? is '("is" "are"))
             (not (member? value '("more" "less" "exactly" "not"))))
        [(cond
          (re-matches #"^[0-9.]*$" value)(list '= (list 'get-int 'cell (keyword property)) (read-string value))
          true (list '= (list (keyword property) 'cell) (keyword value)))
         rest]))

(defn parse-not-condition [[property is not & rest]]
  (cond (and (member? is '("is" "are")) (= not "not"))
        (let [partial (parse-simple-condition (cons property (cons is rest)))]
          (cond partial
                (let [[condition remainder] partial]
                  [(list 'not condition) remainder])))))

(defn parse-simple-condition
  "Parse conditions of the form '[property] [comparison] [value]'."
  [tokens]
  (or (parse-is-condition tokens)
      (parse-not-condition tokens)
      (parse-less-condition tokens)
      (parse-more-condition tokens)))

(defn parse-disjunction-condition
  "Parse '... or [condition]' from `tokens`, there `left` is the already parsed first disjunct."
  [left tokens]
  (let [partial (parse-conditions tokens)]
    (if
       partial
           (let [[right remainder] partial]
             [(list 'or left right) remainder]))))

(defn parse-conjunction-condition
  "Parse '... and [condition]' from `tokens`, there `left` is the already parsed first conjunct."
  [left tokens]
  (let [partial (parse-conditions tokens)]
    (if partial
           (let [[right remainder] partial]
             [(list 'and left right) remainder]
           ))))

(defn parse-conditions
  "Parse conditions from `tokens`, where conditions may be linked by either 'and' or 'or'."
  [tokens]
  (let [partial (parse-simple-condition tokens)]
    (if partial
           (let [[left [next & remainder]] partial]
             (cond
              (= next "and") (parse-conjunction-condition left remainder)
              (= next "or") (parse-disjunction-condition left remainder)
              true partial)
              ))))

(defn parse-left-hand-side
 "Parse the left hand side ('if...') of a production rule."
  [tokens]
  (if
   (= (first tokens) "if")
   (parse-conditions (rest tokens))))

(defn parse-arithmetic-action [previous [prop1 should be prop2 operator value & rest]]
  (if (and (= should "should")
           (= be "be")
           (member? operator '("+" "-" "*" "/")))
    [(list 'merge (or previous 'cell)
           {(keyword prop1) (list (symbol operator) (list 'get-int 'cell (keyword prop2))
                                  (cond
                                     (re-matches #"^[0-9.]*$" value) (read-string value)
                                     true (list 'get-int 'cell (keyword value))))}) rest]))

(defn parse-set-action [previous [property should be value & rest]]
  (if (and (= should "should") (= be "be"))
    [(list 'merge (or previous 'cell)
           {(keyword property) (cond (re-matches #"^[0-9.]*$" value) (read-string value) true (keyword value))}) rest]))

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

(defn parse-rule [line]
  (cond
   (string? line) (parse-rule (split (triml line) #"\s+"))
   true (let [[left remainder] (parse-left-hand-side line)
              [right junk] (parse-right-hand-side remainder)]
          ;; there shouldn't be any junk (should be null)
          (list 'fn ['cell 'world] (list 'if left right))
          )))

