(ns ^{:doc "parse multiple rules from a stream, possibly a file."
      :author "Simon Brooke"}
  mw-parser.bulk
  (:use mw-parser.declarative
        mw-engine.utils
        clojure.java.io
        [clojure.string :only [split trim]])
  (:import (java.io BufferedReader StringReader)))

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
  (map #(compile-rule % true) (remove comment? (split string #"\n"))))

(defn compile-file
  "Compile each non-comment line of the file indicated by this `filename` into
   an executable anonymous function, and return the sequence of such functions."
  [filename]
  (compile-string (slurp filename)))
