(ns parser.core
  (:require [instaparse.core :as insta]))

;; grammar conventions:
;;
;;   - CamelCase is used for NonTerminals
;;   - snake_case is used for terminals
;;
;;   when a NonTerminal introduces a new terminal, it is immediately defined
;;   after that rule.
;;
;;   NonTerminals are defined in the order they are introduced, unless they are
;;   common to many NonTerminals. In that case, they are defined at the bottom
;;   of the grammar.
;;


(def relational-algebra-parser
  (insta/parser
   "S                = RAExpression

    RAExpresssion    = '(' RAExpression ')'                               |
                       '(' RAExpression ')' TopLevelOperator RAexpression |
                       RelationName                                       |
                       RelationName MidLevelOperator RAExpression         |
                       TopLevelOperator RAExpression                      |
                       TopLevelOperator RAExpression MidLevelOperator RAExpression

    TopLevelOperator = Projection     |
                       Selection      |
                       RenameRelation |
                       RenameColumn   |
                       OrderBy        |
                       GroupBy        |

    MidLevelOperator = Intersection |
                       Union        |
                       Division     |
                       Subtraction  |
                       CrossProduct |
                       NaturalJoin
 "))
