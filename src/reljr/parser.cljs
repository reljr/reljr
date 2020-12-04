(ns reljr.parser
  (:require [instaparse.core :as insta])
  (:require-macros [reljr.parser :refer [read-bnf-file]]))

(def full-relational-algebra-parser
  (insta/parser (read-bnf-file)))

(defn relational-algebra-parser [text]
  (full-relational-algebra-parser text :start :RAExpression))
