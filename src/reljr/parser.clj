(ns reljr.parser
  (:require [clojure.java.io :as io]))

;; this is a macro because of cljs not having access to clojure.java.io at runtime
(defmacro read-bnf-file []
  (list 'quote (slurp (io/resource "RAParser.bnf"))))
