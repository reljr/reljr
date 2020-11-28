(ns reljr.parser
  (:require [instaparse.core :as insta]))

(def full-relational-algebra-parser
  (insta/parser (clojure.java.io/resource "RAParser.bnf")))

(defn relational-algebra-parser [text]
  (full-relational-algebra-parser text :start :RAExpression))
