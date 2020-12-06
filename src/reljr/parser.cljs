(ns reljr.parser
  (:require [instaparse.core :as insta])
  (:require-macros [reljr.parser :refer [read-bnf-file]]))

(defonce grammar
  (read-bnf-file))

(def full-relational-algebra-parser
  (insta/parser grammar))

(defn relational-algebra-parser [text]
  (full-relational-algebra-parser text :start :RAExpression))

(defn full-relational-algebra-parses [text]
  (into #{} (insta/parses full-relational-algebra-parser text :start :RAExpression)))
