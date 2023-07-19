(ns ^{:doc "Display parse errors in a format which makes it easy for the user
      to see where the error occurred."
      :author "Simon Brooke"}
  mw-parser.errors)

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


;; error thrown when an attempt is made to set a reserved property
;; error thrown when a rule cannot be parsed. Slots are for
;; (1) rule text
;; (2) cursor showing where in the rule text the error occurred
;; (3) the reason for the error
(def bad-parse-error "I did not understand:\n  '%s'\n  %s\n  %s")

(defn- explain-parse-error-reason
  "Attempt to explain the reason for the parse error."
  [reason]
  (str "Expecting one of (" (apply str (map #(str (:expecting %) " ") reason)) ")"))

(defn- parser-error-to-map
  [parser-error]
  (let [m (reduce (fn [map item](merge map {(first item)(second item)})) {} parser-error)
        reason (map
                 #(reduce (fn [map item] (merge {(first item) (second item)} map)) {} %)
                 (:reason m))]
    (merge m {:reason reason})))

(defn throw-parse-exception
  "Construct a helpful error message from this `parser-error`, and throw an exception with that message."
  [parser-error]
  (assert (coll? parser-error) "Expected a paser error structure?")
  (let
    [
      ;; the error structure is a list, such that each element is a list of two items, and
      ;; the first element in each sublist is a keyword. Easier to work with it as a map
     error-map (parser-error-to-map parser-error)
     text (:text error-map)
     reason (explain-parse-error-reason (:reason error-map))
      ;; rules have only one line, by definition; we're interested in the column
     column (if (:column error-map)(:column error-map) 0)
      ;; create a cursor to point to that column
     cursor (apply str (reverse (conj (repeat column " ") "^")))
     message (format bad-parse-error text cursor reason)
     ]
  (throw (Exception. message))))
