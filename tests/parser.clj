(ns parser.core-test
  (:require  [clojure.test :as t]
             [parser.core :as p]
             [instaparse.core :as insta]))

(t/deftest RAProjection
  (t/is (= (p/relational-algebra-parser "π surname ( Customer )")
           '([:Projection [:Column "surname"] "Customer"])))
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
  (t/is (= (p/relational-algebra-parser "tau a (R)")
           '([:OrderBy [:Column "a"] "R"])))
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

(t/deftest RAComplexExamples
  (t/is
   (=
    (p/relational-algebra-parser "pi Suppliers.sname (Suppliers × (π Catalog.sid (σ color='red' (Parts × Catalog))))")
    '([:Projection
       [:Column "Suppliers" "sname"]
       [:CrossProduct
        "Suppliers"
        [:Projection
         [:Column "Catalog" "sid"]
         [:Selection
          [:EqualsExpr [:Column "color"] [:string "'red'"]]
          [:CrossProduct "Parts" "Catalog"]]]]])))
  (t/is
   (=
    (p/relational-algebra-parser " π Catalog.sid (σ num_red_parts=red_count (γ Catalog.sid, num_red_parts; count(Catalog.sid) -> red_count (π Catalog.sid, Catalog.pid, Parts.color, num_red_parts (σ Parts.color = 'red' and Catalog.pid = Parts.pid (Catalog × Parts * (gamma count(Parts.pid) -> num_red_parts  (σ Parts.color='red' (Parts)))))))) ")
    '([:Projection
       [:Column "Catalog" "sid"]
       [:Selection
        [:EqualsExpr [:Column "num_red_parts"] [:Column "red_count"]]
        [:GroupBy
         [:Column "Catalog" "sid"]
         [:Column "num_red_parts"]
         [:AggregateCount [:Column "Catalog" "sid"] "red_count"]
         [:Projection
          [:Column "Catalog" "sid"]
          [:Column "Catalog" "pid"]
          [:Column "Parts" "color"]
          [:Column "num_red_parts"]
          [:Selection
           [:AndExpr
            [:EqualsExpr [:Column "Parts" "color"] [:string "'red'"]]
            [:EqualsExpr [:Column "Catalog" "pid"] [:Column "Parts" "pid"]]]
           [:CrossProduct
            "Catalog"
            [:CrossProduct
             "Parts"
             [:GroupBy
              [:AggregateCount [:Column "Parts" "pid"] "num_red_parts"]
              [:Selection
               [:EqualsExpr [:Column "Parts" "color"] [:string "'red'"]]
               "Parts"]]]]]]]]])))
  (t/is
   (= (p/relational-algebra-parser " π Employees.ename ((σ planecount ≥ 2 (γ Employees.ename ; count(Certified.aid) -> planecount (σ Aircraft.cruisingrange > 1000 (Certified natural join Employees ⋈ Aircraft)))) natural join σ Aircraft.aname='Boeing' (Certified natural join Aircraft ⋈ Employees)) ")
      '([:Projection
         [:Column "Employees" "ename"]
         [:NaturalJoin
          [:Selection
           [:GreaterEqualExpr [:Column "planecount"] [:number "2"]]
           [:GroupBy
            [:Column "Employees" "ename"]
            [:AggregateCount [:Column "Certified" "aid"] "planecount"]
            [:Selection
             [:GreaterExpr [:Column "Aircraft" "cruisingrange"] [:number "1000"]]
             [:NaturalJoin "Certified" [:NaturalJoin "Employees" "Aircraft"]]]]]
          [:Selection
           [:EqualsExpr [:Column "Aircraft" "aname"] [:string "'Boeing'"]]
           [:NaturalJoin "Certified" [:NaturalJoin "Aircraft" "Employees"]]]]])))

  (t/is (= (p/relational-algebra-parser " pi Employees.ename, Employees.salary (sigma Employees.salary >= pilotavgsalary (((pi Employees.eid (pi Employees.eid (Employees)) - (pi Certified.eid (Certified))) cross join pi Employees.salary Employees) natural join Employees natural join gamma avg(Employees.salary) -> pilotavgsalary (Certified natural join Employees))) ")
           '([:Projection
              [:Column "Employees" "ename"]
              [:Column "Employees" "salary"]
              [:Selection
               [:GreaterEqualExpr
                [:Column "Employees" "salary"]
                [:Column "pilotavgsalary"]]
               [:NaturalJoin
                [:CrossProduct
                 [:Projection
                  [:Column "Employees" "eid"]
                  [:Subtraction
                   [:Projection [:Column "Employees" "eid"] "Employees"]
                   [:Projection [:Column "Certified" "eid"] "Certified"]]]
                 [:Projection [:Column "Employees" "salary"] "Employees"]]
                [:NaturalJoin
                 "Employees"
                 [:GroupBy
                  [:AggregateAvg [:Column "Employees" "salary"] "pilotavgsalary"]
                  [:NaturalJoin "Certified" "Employees"]]]]]])))
  (t/is (= (p/relational-algebra-parser
            " pi Catalog.pid (sigma smax=scount ((gamma count(sid)-> smax (Suppliers)) * (gamma Catalog.pid; count(Catalog.sid) -> scount (pi Catalog.sid, Catalog.pid (Suppliers * (sigma Catalog.pid = Parts.pid && Catalog.cost < 200 (Catalog * Parts)))))))")
           '([:Projection
              [:Column "Catalog" "pid"]
              [:Selection
               [:EqualsExpr [:Column "smax"] [:Column "scount"]]
               [:CrossProduct
                [:GroupBy [:AggregateCount [:Column "sid"] "smax"] "Suppliers"]
                [:GroupBy
                 [:Column "Catalog" "pid"]
                 [:AggregateCount [:Column "Catalog" "sid"] "scount"]
                 [:Projection
                  [:Column "Catalog" "sid"]
                  [:Column "Catalog" "pid"]
                  [:CrossProduct
                   "Suppliers"
                   [:Selection
                    [:AndExpr
                     [:EqualsExpr [:Column "Catalog" "pid"] [:Column "Parts" "pid"]]
                     [:LessExpr [:Column "Catalog" "cost"] [:number "200"]]]
                    [:CrossProduct "Catalog" "Parts"]]]]]]]]))))
