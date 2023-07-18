(ns ^{:doc "Utilities used in more than one namespace within the parser."
      :author "Simon Brooke"}
  mw-parser.utils 
  (:require [clojure.string :refer [trim]]
            [mw-engine.utils :refer [member?]]))

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

(defn suitable-fragment?
  "Return `true` if `tree-fragment` appears to be a tree fragment of the expected `type`."
  [tree-fragment type]
  (and (coll? tree-fragment)
       (keyword? type)
       (= (first tree-fragment) type)))

(defn rule?
  "Return true if the argument appears to be a parsed rule tree, else false."
  [maybe-rule]
  (suitable-fragment? maybe-rule :RULE))

(defn TODO
  "Marker to indicate I'm not yet finished!"
  [message]
  message)



(defn assert-type
  "If `tree-fragment` is not a tree fragment of the expected `type`, throw an exception."
  [tree-fragment type]
  (assert (suitable-fragment? tree-fragment type)
          (throw (ex-info (format "Expected a %s fragment" type)
                          {:actual tree-fragment
                           :expected type}))))


(defn search-tree
  "Return the first element of this tree which has this tag in a depth-first, left-to-right search"
  [tree tag]
  (cond 
    (= (first tree) tag) tree
    :else (first
            (remove nil?
                    (map
                      #(search-tree % tag)
                      (filter coll? (rest tree)))))))
