(ns mw-parser.bulk-test
  (:use clojure.java.io)
  (:require [clojure.test :refer :all]
            [mw-parser.bulk :refer :all]))

(deftest bulk-parsing-test
         (testing "Bulk (file) parsing and compilation"
                  (is (= (count (parse-file (as-file "resources/rules.txt"))) 15)
                      "Should parse all rules and throw no exceptions")
                  (is (empty? 
                        (remove #(= % 'fn) 
                                (map first 
                                     (parse-file 
                                       (as-file "resources/rules.txt")))))
                      "all parsed rules should be lambda sexprs")
                  (is (= (count (compile-file (as-file "resources/rules.txt"))) 15)
                      "Should compile all rules and throw no exceptions")
                  (is (empty?
                        (remove ifn? 
                                (map first 
                                     (compile-file 
                                       (as-file "resources/rules.txt")))))
                      "all compiled rules should be ifns")
                  ))
