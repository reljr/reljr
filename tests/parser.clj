(ns test-parser
  (:require  [clojure.test :as t]
             [parser.core :as p]
             [instaparse.core :as insta]))

(p/relational-algebra-parser "sigma firstname = 'Bob'( Customer )")

(t/testing "Selection"
  (t/is (= (p/relational-algebra-parser "sigma firstname = 'Bob'( Customer )")
           '([:Selection [:EqualsExpr [:Column "firstname"] [:string "'Bob'"]] "Customer"])))
  (t/is (= (p/relational-algebra-parser "σ firstname = 'Bob' or firstname = 'Alice' (Customer)")
           '([:Selection
              [:OrExpr
               [:EqualsExpr [:Column "firstname"] [:string "'Bob'"]]
               [:EqualsExpr [:Column "firstname"] [:string "'Alice'"]]]
              "Customer"])))
  (t/is (= (p/relational-algebra-parser "σ (id > 10 and id < 100) or id = 42 ( Customer )")
           '([:Selection
              [:OrExpr
               [:AndExpr
                [:GreaterExpr [:Column "id"] [:number "10"]]
                [:LessExpr [:Column "id"] [:number "100"]]]
               [:EqualsExpr [:Column "id"] [:number "42"]]]
              "Customer"]))))

(t/testing "Projection" (t/is (= (p/relational-algebra-parser "π Customer.firstname, surname ( Customer )")
                                 '([:Projection [:Column "Customer" "firstname"] [:Column "surname"] "Customer"]))))

(t/testing "Rename Column"
  (t/is (= (p/relational-algebra-parser "ρ myId->id, foobar->firstname (π id, firstname ( Customer ) )")
           '([:RenameColumn
              [:Column "myId"]
              "id"
              [:Column "foobar"]
              "firstname"
              [:Projection [:Column "id"] [:Column "firstname"] "Customer"]]))))

(p/relational-algebra-parser "π a.id, a.firstname ( ρ a ( Customer ) )")

(t/run-all-tests)
