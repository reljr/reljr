(ns parser.core-test
  (:require  [clojure.test :as t]
             [parser.core :as p]
             [instaparse.core :as insta]))

(t/deftest RAProjection
  (t/is (= (p/relational-algebra-parser "π Customer.firstname, surname ( Customer )")
           '([:Projection [:Column "Customer" "firstname"] [:Column "surname"] "Customer"]))))

(t/deftest RASelection
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

(p/relational-algebra-parser "π a.id, a.firstname ( ρ a ( Customer ) )")

(t/deftest RARenameRelation
  (t/is (=
         (p/relational-algebra-parser "π a.id, a.firstname ( ρ a ( Customer ) )")
         '([:Projection
            [:Column "a" "id"]
            [:Column "a" "firstname"]
            [:RenameRelation "a" "Customer"]]))))

(t/deftest RARenameColumn
  (t/is (= (p/relational-algebra-parser "ρ myId->id, foobar->firstname (π id, firstname ( Customer ) )")
           '([:RenameColumn
              [:Column "myId"]
              "id"
              [:Column "foobar"]
              "firstname"
              [:Projection [:Column "id"] [:Column "firstname"] "Customer"]]))))

(t/deftest RAOrderBy
  (t/is (= (p/relational-algebra-parser "tau a asc, b desc (R)")
           '([:OrderBy [:Column "a"] [:Column "b"] "R"])))
  (t/is (= (p/relational-algebra-parser "τ firstname desc (π id, firstname ( Customer ) )")
           '([:OrderBy
              [:Column "firstname"]
              [:Projection [:Column "id"] [:Column "firstname"] "Customer"]]))))

(t/deftest RAGroupBy
  (t/is (= (p/relational-algebra-parser "gamma a ; count(*)->x  ( R ) ")
           '([:GroupBy [:Column "a"] [:AggregateCountStar "x"] "R"])))
  (t/is (= (p/relational-algebra-parser "γ a, b ; sum(c)->x ( Customer )")
           '([:GroupBy
              [:Column "a"]
              [:Column "b"]
              [:AggregateSum [:Column "c"] "x"]
              "Customer"])))
  (t/is (= (p/relational-algebra-parser "γ a, b ; sum(c)->x, sum(d)->y ( Customer )")
           '([:GroupBy
              [:Column "a"]
              [:Column "b"]
              [:AggregateSum [:Column "c"] "x"]
              [:AggregateSum [:Column "d"] "y"]
              "Customer"])))
  (t/is (= (p/relational-algebra-parser "γ a, b ; count(*)-> z, count(c)->a, min(c)->b, max(c)->c, sum(c)->d, avg(d)->e ( Customer )")
           '([:GroupBy
              [:Column "a"]
              [:Column "b"]
              [:AggregateCountStar "z"]
              [:AggregateCount [:Column "c"] "a"]
              [:AggregateMin [:Column "c"] "b"]
              [:AggregateMax [:Column "c"] "c"]
              [:AggregateSum [:Column "c"] "d"]
              [:AggregateAvg [:Column "d"] "e"]
              "Customer"]))))

(t/deftest RACrossProduct (t/is (= (p/relational-algebra-parser "pi a B * pi c D")
                                   '([:Projection
                                      [:Column "a"]
                                      [:CrossProduct "B" [:Projection [:Column "c"] "D"]]]))))

(t/deftest RANaturalJoin (t/is (= (p/relational-algebra-parser "ρ a ( Customer )⋈ sigma a.name < b.name ( ρ b ( Customer ) )")
                                  '([:RenameRelation
                                     "a"
                                     [:NaturalJoin
                                      "Customer"
                                      [:Selection
                                       [:LessExpr [:Column "a" "name"] [:Column "b" "name"]]
                                       [:RenameRelation "b" "Customer"]]]]))))

(t/deftest RADivision
  (t/is
   (= (p/relational-algebra-parser " ( Customer ) ÷ ( Customer ) ")
      '([:Division "Customer" "Customer"]))))

(t/deftest RAIntersection
  (t/is
   (= (p/relational-algebra-parser "( Customer ) ∩ ( Customer )")
      '([:Intersection "Customer" "Customer"]))))

(t/deftest RAUnion
  (t/is
   (= (p/relational-algebra-parser " ( Customer ) ∪ ( Customer ) ")
      '([:Union "Customer" "Customer"]))))

(t/deftest RASubtraction
  (t/is
   (=
    (p/relational-algebra-parser " ( pi firstname ( Customer ) ) - ( rho lastname->test ( pi lastname ( Customer ) ) ) ")
    '([:Subtraction
       [:Projection [:Column "firstname"] "Customer"]
       [:RenameColumn
        [:Column "lastname"]
        "test"
        [:Projection [:Column "lastname"] "Customer"]]]))))