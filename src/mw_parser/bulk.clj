;; parse multiple rules from a stream, possibly a file - although the real
;; objective is to parse rules out of a block of text from a textarea

(ns mw-parser.bulk
  (:use mw-parser.core
        mw-engine.utils
        clojure.java.io
        [clojure.string :only [split trim]])
  (:import (java.io BufferedReader StringReader)))

(defn comment? 
  "Is this `line` a comment?"
  [line]
  (or (empty? (trim line)) (member? (first line) '(nil \# \;))))

(defn parse-string 
  "Parse rules from successive lines in this `string`, assumed to have multiple
   lines delimited by the new-line character. Return a list of S-expressions."
  [string]
        ;; TODO: tried to do this using with-open, but couldn't make it work.
  (map parse-rule (remove comment? (split string #"\n"))))

(defn parse-file 
  "Parse rules from successive lines in the file loaded from this `filename`.
   Return a list of S-expressions."
  [filename]
  (parse-string (slurp filename)))

(defn compile-string
  "Compile each non-comment line of this `string` into an executable anonymous 
   function, and return the sequence of such functions."
  [string]
  (map #(compile-rule % true) (remove comment? (trim (split string #"\n")))))

(defn compile-file 
  "Compile each non-comment line of the file indicated by this `filename` into 
   an executable anonymous function, and return the sequence of such functions."
  [filename]
  (compile-string (slurp filename)))
