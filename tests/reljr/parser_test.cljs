(ns reljr.parser-test
  (:require [cljs.test :refer-macros [deftest is]]
            [reljr.parser :as p]))

(defn parse [text]
  (first (p/relational-algebra-parser text)))

(deftest RAProjection
  (is (= [:Projection [:Column "surname"] "Customer"]
         (parse "π surname ( Customer )")))
  (is (= [:Projection [:Column "Customer" "firstname"] [:Column "surname"] "Customer"]
         (parse "π Customer.firstname, surname ( Customer )"))))

(deftest RASelection
  (is (= [:Selection [:EqualsExpr [:Column "firstname"] [:string "Bob"]] "Customer"]
         (parse "sigma firstname = 'Bob'( Customer )")))
  (is (= [:Selection
          [:OrExpr
           [:EqualsExpr [:Column "firstname"] [:string "Bob"]]
           [:EqualsExpr [:Column "firstname"] [:string "Alice"]]]
          "Customer"]
         (parse "σ firstname = 'Bob' or firstname = 'Alice' (Customer)")))
  (is (= [:Selection
          [:OrExpr
           [:AndExpr
            [:GreaterExpr [:Column "id"] [:number "10"]]
            [:LessExpr [:Column "id"] [:number "100"]]]
           [:EqualsExpr [:Column "id"] [:number "42"]]]
          "Customer"]
         (parse "σ (id > 10 and id < 100) or id = 42 ( Customer )"))))

(deftest RARenameRelation
  (is (= [:Projection
          [:Column "a" "id"]
          [:Column "a" "firstname"]
          [:RenameRelation "a" "Customer"]]
         (parse "π a.id, a.firstname ( ρ a ( Customer ) )"))))

(deftest RARenameColumn
  (is (= [:RenameColumn
          [:Column "myId"]
          "id"
          [:Column "foobar"]
          "firstname"
          [:Projection [:Column "id"] [:Column "firstname"] "Customer"]]
         (parse "ρ myId->id, foobar->firstname (π id, firstname ( Customer ) )"))))

(deftest RAOrderBy
  (is (= [:OrderBy [:AscendingColumn [:Column "a"]] "R"]
         (parse "tau a asc (R)")))
  (is (= [:OrderBy
          [:AscendingColumn [:Column "a"]]
          [:DescendingColumn [:Column "b"]]
          "R"]
         (parse "tau a asc, b desc (R)")))
  (is (= [:OrderBy
          [:DescendingColumn [:Column "firstname"]]
          [:Projection [:Column "id"] [:Column "firstname"] "Customer"]]
         (parse "τ firstname desc (π id, firstname ( Customer ) )"))))

(deftest RAGroupBy
  (is (= [:GroupBy [:Column "a"] [:AggregateCountStar "x"] "R"]
         (parse "gamma a ; count(*)->x  ( R ) ")))
  (is (= [:GroupBy
          [:Column "a"]
          [:Column "b"]
          [:AggregateSum [:Column "c"] "x"]
          "Customer"]
         (parse "γ a, b ; sum(c)->x ( Customer )")))
  (is (= [:GroupBy
          [:Column "a"]
          [:Column "b"]
          [:AggregateSum [:Column "c"] "x"]
          [:AggregateSum [:Column "d"] "y"]
          "Customer"]
         (parse "γ a, b ; sum(c)->x, sum(d)->y ( Customer )")))
  (is (= [:GroupBy
          [:Column "a"]
          [:Column "b"]
          [:AggregateCountStar "z"]
          [:AggregateCount [:Column "c"] "a"]
          [:AggregateMin [:Column "c"] "b"]
          [:AggregateMax [:Column "c"] "c"]
          [:AggregateSum [:Column "c"] "d"]
          [:AggregateAvg [:Column "d"] "e"]
          "Customer"]
         (parse "γ a, b ; count(*)-> z, count(c)->a, min(c)->b, max(c)->c, sum(c)->d, avg(d)->e ( Customer )"))))

(deftest RACrossProduct
  (is (= [:CrossProduct
          [:Projection [:Column "a"] "B"]
          [:Projection [:Column "c"] "D"]]
         (parse "pi a B * pi c D"))))

(deftest RAInnerJoin
  (is (= [:InnerJoin
          [:RenameRelation "a" "Customer"]
          [:LessExpr [:Column "a" "name"] [:Column "b" "name"]]
          [:RenameRelation "b" "Customer"]]
         (parse "ρ a ( Customer )⋈ a.name < b.name ( ρ b ( Customer ) )"))))

(deftest RANaturalJoin
  (is (= [:NaturalJoin
          [:RenameRelation "a" "Customer"]
          [:Selection
           [:LessExpr [:Column "a" "name"] [:Column "b" "name"]]
           [:RenameRelation "b" "Customer"]]]
         (parse "ρ a ( Customer )⋈ sigma a.name < b.name ( ρ b ( Customer ) )"))))

(deftest RADivision
  (is (= [:Division "Customer" "Customer"]
         (parse " ( Customer ) ÷ ( Customer ) "))))

(deftest RAIntersection
  (is (= [:Intersection "Customer" "Customer"]
         (parse "( Customer ) ∩ ( Customer )"))))

(deftest RAUnion
  (is (= [:Union "Customer" "Customer"]
         (parse " ( Customer ) ∪ ( Customer ) "))))

(deftest RASubtraction
  (is (= [:Subtraction
          [:Projection [:Column "firstname"] "Customer"]
          [:RenameColumn
           [:Column "lastname"]
           "test"
           [:Projection [:Column "lastname"] "Customer"]]]
         (parse " ( pi firstname ( Customer ) ) - ( rho lastname->test ( pi lastname ( Customer ) ) ) "))))

(deftest RAComplexExamples
  (is (= [:Projection
          [:Column "Suppliers" "sname"]
          [:CrossProduct
           "Suppliers"
           [:Projection
            [:Column "Catalog" "sid"]
            [:Selection
             [:EqualsExpr [:Column "color"] [:string "red"]]
             [:CrossProduct "Parts" "Catalog"]]]]]
         (parse "pi Suppliers.sname (Suppliers × (π Catalog.sid (σ color='red' (Parts × Catalog))))")))
  (is (= [:Projection
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
               [:EqualsExpr [:Column "Parts" "color"] [:string "red"]]
               [:EqualsExpr [:Column "Catalog" "pid"] [:Column "Parts" "pid"]]]
              [:CrossProduct
               [:CrossProduct "Catalog" "Parts"]
               [:GroupBy
                [:AggregateCount [:Column "Parts" "pid"] "num_red_parts"]
                [:Selection
                 [:EqualsExpr [:Column "Parts" "color"] [:string "red"]]
                 "Parts"]]]]]]]]
         (parse " π Catalog.sid (σ num_red_parts=red_count (γ Catalog.sid, num_red_parts; count(Catalog.sid) -> red_count (π Catalog.sid, Catalog.pid, Parts.color, num_red_parts (σ Parts.color = 'red' and Catalog.pid = Parts.pid (Catalog × Parts * (gamma count(Parts.pid) -> num_red_parts  (σ Parts.color='red' (Parts)))))))) ")))
  (is (= [:Projection
          [:Column "Employees" "ename"]
          [:NaturalJoin
           [:Selection
            [:GreaterEqualExpr [:Column "planecount"] [:number "2"]]
            [:GroupBy
             [:Column "Employees" "ename"]
             [:AggregateCount [:Column "Certified" "aid"] "planecount"]
             [:Selection
              [:GreaterExpr [:Column "Aircraft" "cruisingrange"] [:number "1000"]]
              [:NaturalJoin [:NaturalJoin "Certified" "Employees"] "Aircraft"]]]]
           [:Selection
            [:EqualsExpr [:Column "Aircraft" "aname"] [:string "Boeing"]]
            [:NaturalJoin [:NaturalJoin "Certified" "Aircraft"] "Employees"]]]]
         (parse " π Employees.ename ((σ planecount ≥ 2 (γ Employees.ename ; count(Certified.aid) -> planecount (σ Aircraft.cruisingrange > 1000 (Certified natural join Employees ⋈ Aircraft)))) natural join σ Aircraft.aname='Boeing' (Certified natural join Aircraft ⋈ Employees)) ")))

  (is (= [:Projection
          [:Column "Employees" "ename"]
          [:Column "Employees" "salary"]
          [:Selection
           [:GreaterEqualExpr
            [:Column "Employees" "salary"]
            [:Column "pilotavgsalary"]]
           [:NaturalJoin
            [:NaturalJoin
             [:CrossProduct
              [:Subtraction
               [:Projection
                [:Column "Employees" "eid"]
                [:Projection [:Column "Employees" "eid"] "Employees"]]
               [:Projection [:Column "Certified" "eid"] "Certified"]]
              [:Projection [:Column "Employees" "salary"] "Employees"]]
             "Employees"]
            [:GroupBy
             [:AggregateAvg [:Column "Employees" "salary"] "pilotavgsalary"]
             [:NaturalJoin "Certified" "Employees"]]]]]
         (parse " pi Employees.ename, Employees.salary (sigma Employees.salary >= pilotavgsalary (((pi Employees.eid (pi Employees.eid (Employees)) - (pi Certified.eid (Certified))) cross join pi Employees.salary Employees) natural join Employees natural join gamma avg(Employees.salary) -> pilotavgsalary (Certified natural join Employees))) ")))
  (is (= [:Projection
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
                [:CrossProduct "Catalog" "Parts"]]]]]]]]
         (parse " pi Catalog.pid (sigma smax=scount ((gamma count(sid)-> smax (Suppliers)) * (gamma Catalog.pid; count(Catalog.sid) -> scount (pi Catalog.sid, Catalog.pid (Suppliers * (sigma Catalog.pid = Parts.pid && Catalog.cost < 200 (Catalog * Parts)))))))"))))
