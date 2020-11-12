(ns parser.core
  (:require [instaparse.core :as insta]))

(def relational-algebra-parser
  (insta/parser (clojure.java.io/resource "RAParser.bnf")))
