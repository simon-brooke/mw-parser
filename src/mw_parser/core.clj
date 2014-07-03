;; A very simple parser which parses production rules of the following forms:
;;
;; "if altitude is less than 100 and state is forest then state should be climax and deer should be 3"
;; "if altitude is 100 and fertility is 25 then state should be heath"
;; "if altitude is 100 or fertility is 25 then state should be heath"
;;
;; it generates rules in the form expected by mw-engine.core

(ns mw-parser.core
  (:use mw-engine.utils
        [clojure.string :only [split triml]]))

(declare parse-conditions)
(declare parse-not-condition)
(declare parse-simple-condition)

(defn parse-less-condition [[property is less than value & rest]]
  (cond (and (= is "is") (= less "less") (= than "than"))
        [(list '< (list 'get-int 'cell (keyword property)) value)
         rest]))

(defn parse-more-condition [[property is more than value & rest]]
  (cond (and (= is "is") (= more "more") (= than "than"))
        [(list '> (list 'get-int 'cell (keyword property)) value)
         :remainder rest]))

(defn parse-is-condition
  "Parse clauses of the form 'x is y', but not 'x is more than y' or 'x is less than y'.
   It is necessary to disambiguate whether value is a numeric or keyword."
  [[property is value & rest]]
  (cond (and (= is "is")
             (not (member? value '("more" "less" "exactly" "not"))))
        [(cond
          (re-matches #"^[0-9]*$" value)(list '= (list 'get-int 'cell (keyword property)) (. Integer parseInt value))
          true (list '= (list (keyword property) 'cell) (keyword value)))
         rest]))

(defn parse-not-condition [[property is not & rest]]
  (cond (and (= is "is") (= not "not"))
        (let [partial (parse-simple-condition (cons property (cons is rest)))]
          (cond partial
                (let [[condition remainder] partial]
                  [(list 'not condition) remainder])))))

(defn parse-simple-condition
  "Parse conditions of the form '[property] [comparison] [value]'."
  [tokens]
  (or (parse-is-condition tokens)
      (parse-not-condition tokens)
      (parse-exactly-condition tokens)
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

(defn parse-simple-action [previous [property should be value & rest]]
  (if (and (= should "should") (= be "be"))
    [(list 'merge (or previous 'cell)
           {(keyword property) (cond (re-matches #"^[0-9]*$" value) (. Integer parseInt value) true (keyword value))}) rest])
  )

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

