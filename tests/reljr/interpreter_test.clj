(ns reljr.interpreter-test
  (:require  [clojure.test :as t])
  (:require [reljr.interpreter :as i])
  (:require [reljr.parser :as p]))

(def table-foo
  {"Foo" #{{:R/x 5 :S/x 6.3 :R/y 6} {:R/x 3 :S/x 9.4 :R/y 2}}})

(def table-bar
  {"bar" #{{:R/a 1 :R/b "a" :R/c "d"}
           {:R/a 3 :R/b "c" :R/c "c"}
           {:R/a 4 :R/b "d" :R/c "f"}
           {:R/a 5 :R/b "d" :R/c "b"}
           {:R/a 6 :R/b "e" :R/c "f"}}})

(def table-baz
  {"baz"
   #{{:R/x 8 :S/x 7 :R/y 2}
     {:R/x 45 :S/x 32 :R/y 2}
     {:R/x 99 :S/x 12 :R/y 84}}})

(def parse #(first (p/relational-algebra-parser %)))

(t/deftest Resolve-Columns
  (t/is (= (i/resolve-columns [[:Column "foo"]] #{:R/foo})
           [:R/foo]))
  (t/is (= (i/resolve-columns [[:Column "foo"] [:Column "Bar" "foo"]] #{:R/foo})
           [:R/foo :Bar/foo])))

(t/deftest Evaluate
  (t/is (= (i/evaluate "Foo" {"Foo" #{{}}})
           #{{}}))
  (t/testing "projection"
    (t/is (= (i/evaluate (parse "pi x Foo") table-foo)
             #{#:R{:x 5} #:R{:x 3}}))
    (t/is (= (i/evaluate (parse "pi R.x Foo") table-foo)
             #{#:R{:x 5} #:R{:x 3}}))
    (t/is (= (i/evaluate (parse "pi S.x Foo") table-foo)
             #{#:S{:x 6.3} #:S{:x 9.4}})))
  (t/testing "rename column"
    (t/is (= (i/evaluate (parse "rho S.x -> y Foo") table-foo)
             #{{:R/x 5, :R/y 6, :S/y 6.3} {:R/x 3, :R/y 2, :S/y 9.4}})))
  (t/testing "rename relation"
    (t/is (= (i/evaluate (parse "rho Baz Foo") table-foo)
             #{#:Baz{:x 6.3, :y 6} #:Baz{:x 9.4, :y 2}})))
  (t/testing "selection"
    (t/is (= (i/evaluate (parse "sigma R.x > 4 Foo") table-foo)
             #{{:R/x 5, :S/x 6.3, :R/y 6}}))
    (t/is (= (i/evaluate (parse "sigma R.x > 4 and y > 5 Foo") table-foo)
             #{{:R/x 5, :S/x 6.3, :R/y 6}})))
  (t/testing "order by"
    (t/is (= (into [] (i/evaluate (parse "tau R.a asc bar") table-bar))
             [#:R{:a 1, :b "a", :c "d"}
              #:R{:a 3, :b "c", :c "c"}
              #:R{:a 4, :b "d", :c "f"}
              #:R{:a 5, :b "d", :c "b"}
              #:R{:a 6, :b "e", :c "f"}]))
    (t/is (= (into [] (i/evaluate (parse "tau R.c desc bar") table-bar))
             [#:R{:a 4, :b "d", :c "f"}
              #:R{:a 6, :b "e", :c "f"}
              #:R{:a 1, :b "a", :c "d"}
              #:R{:a 3, :b "c", :c "c"}
              #:R{:a 5, :b "d", :c "b"}])))

  (t/testing "group by")
  (t/testing "union"
    (t/is (= (i/evaluate (parse "baz union bar") (merge table-bar table-baz))
             #{#:R{:a 1, :b "a", :c "d"} {:R/x 99, :S/x 12, :R/y 84}
               {:R/x 8, :S/x 7, :R/y 2} #:R{:a 4, :b "d", :c "f"} #:R{:a 3, :b "c", :c "c"}
               {:R/x 45, :S/x 32, :R/y 2} #:R{:a 5, :b "d", :c "b"}
               #:R{:a 6, :b "e", :c "f"}})))

  (t/testing "subtraction"
    (t/is (= (i/evaluate (parse "Foo except bar") (merge table-foo table-bar))
             #{{:R/x 5, :S/x 6.3, :R/y 6} {:R/x 3, :S/x 9.4, :R/y 2}})))

  (t/testing "cross product"
    (t/is (= (i/evaluate (parse "bar * baz") (merge table-bar table-baz))
             #{{:R/a 3, :R/b "c", :R/c "c", :R/x 8, :S/x 7, :R/y 2}
               {:R/a 1, :R/b "a", :R/c "d", :R/x 8, :S/x 7, :R/y 2}
               {:R/a 3, :R/b "c", :R/c "c", :R/x 99, :S/x 12, :R/y 84}
               {:R/a 1, :R/b "a", :R/c "d", :R/x 99, :S/x 12, :R/y 84}
               {:R/a 5, :R/b "d", :R/c "b", :R/x 45, :S/x 32, :R/y 2}
               {:R/a 6, :R/b "e", :R/c "f", :R/x 8, :S/x 7, :R/y 2}
               {:R/a 4, :R/b "d", :R/c "f", :R/x 45, :S/x 32, :R/y 2}
               {:R/a 5, :R/b "d", :R/c "b", :R/x 99, :S/x 12, :R/y 84}
               {:R/a 1, :R/b "a", :R/c "d", :R/x 45, :S/x 32, :R/y 2}
               {:R/a 4, :R/b "d", :R/c "f", :R/x 8, :S/x 7, :R/y 2}
               {:R/a 4, :R/b "d", :R/c "f", :R/x 99, :S/x 12, :R/y 84}
               {:R/a 5, :R/b "d", :R/c "b", :R/x 8, :S/x 7, :R/y 2}
               {:R/a 6, :R/b "e", :R/c "f", :R/x 99, :S/x 12, :R/y 84}
               {:R/a 6, :R/b "e", :R/c "f", :R/x 45, :S/x 32, :R/y 2}
               {:R/a 3, :R/b "c", :R/c "c", :R/x 45, :S/x 32, :R/y 2}})))

  (t/testing "join"
    (t/is (= (i/evaluate (first (reljr.parser/relational-algebra-parser "Foo join (R.x = Q.x) Baz"))
                         {"Foo" #{{:R/x 5 :S/x 2.1 :R/y 6} {:R/x 3 :S/x 2.2 :R/y 2} {:R/x 5 :S/x 2.3 :R/y 7}}
                          "Baz" #{{:Q/x 5 :Q/a 1} {:Q/x 3 :Q/a 2}}})
             #{{:R/x 5, :S/x 2.3, :R/y 7, :Q/x 5, :Q/a 1}
               {:R/x 3, :S/x 2.2, :R/y 2, :Q/x 3, :Q/a 2}
               {:R/x 5, :S/x 2.1, :R/y 6, :Q/x 5, :Q/a 1}})))

  (t/testing "division"
    (= (i/evaluate (first (reljr.parser/relational-algebra-parser "(pi R.x, R.y Foo) / Quux"))
                   {"Foo" #{{:R/x 5 :S/x 2.1 :R/y 6} {:R/x 3 :S/x 2.2 :R/y 2} {:R/x 5 :S/x 2.3 :R/y 7}}
                    "Bar" #{{:R/x 10 :S/x 2.4 :R/y 9} {:R/x 5 :S/x 2.1 :R/y 6}}
                    "Baz" #{{:Q/x 5 :Q/a 1} {:Q/x 3 :Q/a 2}}
                    "Quux" #{{:R/y 6} {:R/y 7}}})
       #{#:R{:x 5}})))
