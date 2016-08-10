(ns mw-parser.utils)


(defn rule?
  "Return true if the argument appears to be a parsed rule tree, else false."
  [maybe-rule]
  (and (coll? maybe-rule) (= (first maybe-rule) :RULE)))


(defn TODO
  "Marker to indicate I'm not yet finished!"
  [message]
  message)


(defn suitable-fragment?
  "Return `true` if `tree-fragment` appears to be a tree fragment of the expected `type`."
  [tree-fragment type]
  (and (coll? tree-fragment)
       (= (first tree-fragment) type)))


(defn assert-type
  "If `tree-fragment` is not a tree fragment of the expected `type`, throw an exception."
  [tree-fragment type]
  (assert (suitable-fragment? tree-fragment type)
          (throw (Exception. (format "Expected a %s fragment" type)))))


(defn search-tree
  "Return the first element of this tree which has this tag in a depth-first, left-to-right search"
  [tree tag]
  (cond
    (= (first tree) tag) tree
    :else (first
            (remove nil?
                    (map
                      #(search-tree % tag)
                      (rest tree))))))
