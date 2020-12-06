(ns reljr.interpreter-test
  (:require [clojure.test :as t]
            [reljr.preprocessor :as rpp]
            [reljr.interpreter :as i]
            [reljr.parser :as p]))

(def table-foo
  {"Foo" #{{:R/x 5 :S/x 6.3 :R/y 6} {:R/x 3 :S/x 9.4 :R/y 2}}})
(def table-foo-prime
  {"Foop" #{{:R/x 5 :S/x 6.3 :R/y 6} {:R/x 7 :S/x 14 :R/y 12}}})

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
(def evaluate #(i/evaluate (rpp/preprocess-query %1 %2) %2))

(t/deftest Evaluate
  (t/is (= (evaluate "Foo" {"Foo" #{{}}})
           #{{}}))
  (t/testing "projection"
    (t/is (= (evaluate (parse "pi x Foo") table-foo)
             #{#:R{:x 5} #:R{:x 3}}))
    (t/is (= (evaluate (parse "pi R.x Foo") table-foo)
             #{#:R{:x 5} #:R{:x 3}}))
    (t/is (= (evaluate (parse "pi S.x Foo") table-foo)
             #{#:S{:x 6.3} #:S{:x 9.4}})))
  (t/testing "rename column"
    (t/is (= (evaluate (parse "rho S.x -> y Foo") table-foo)
             #{{:R/x 5, :R/y 6, :S/y 6.3} {:R/x 3, :R/y 2, :S/y 9.4}})))
  (t/testing "rename relation"
    (t/is (= (evaluate (parse "rho Baz pi S.x, R.y Foo") table-foo)
             #{#:Baz{:x 6.3, :y 6} #:Baz{:x 9.4, :y 2}})))
  (t/testing "selection"
    (t/is (= (evaluate (parse "sigma R.x > 4 Foo") table-foo)
             #{{:R/x 5, :S/x 6.3, :R/y 6}}))
    (t/is (= (evaluate (parse "sigma R.x > 4 and y > 5 Foo") table-foo)
             #{{:R/x 5, :S/x 6.3, :R/y 6}})))
  (t/testing "order by"
    (t/is (= (into [] (evaluate (parse "tau R.a asc bar") table-bar))
             [#:R{:a 1, :b "a", :c "d"}
              #:R{:a 3, :b "c", :c "c"}
              #:R{:a 4, :b "d", :c "f"}
              #:R{:a 5, :b "d", :c "b"}
              #:R{:a 6, :b "e", :c "f"}]))
    (t/is (= (into [] (evaluate (parse "tau R.c desc bar") table-bar))
             [#:R{:a 4, :b "d", :c "f"}
              #:R{:a 6, :b "e", :c "f"}
              #:R{:a 1, :b "a", :c "d"}
              #:R{:a 3, :b "c", :c "c"}
              #:R{:a 5, :b "d", :c "b"}])))

  (t/testing "group by"
    (t/testing "sum"
      (t/is (= (evaluate (parse "gamma sum (y) -> a Foo") table-foo)
               #:R{:a 8}))
      (t/is (= (evaluate (parse "gamma b; sum (a) -> sum bar") table-bar)
               #{#:R{:sum 1, :b "a"}
                 #:R{:sum 3, :b "c"}
                 #:R{:sum 6, :b "e"}
                 #:R{:sum 9, :b "d"}})))
    (t/testing "min"
      (t/is (= (evaluate (parse "gamma min (a) -> min bar") table-bar)
               #:R{:min 1}))
      (t/is (= (evaluate (parse "gamma b; min (a) -> min bar") table-bar)
               #{#:R{:min 4, :b "d"}
                 #:R{:min 6, :b "e"}
                 #:R{:min 3, :b "c"}
                 #:R{:min 1, :b "a"}})))
    (t/testing "max"
      (t/is (= (evaluate (parse "gamma max (a) -> max bar") table-bar)
               #:R{:max 6}))
      (t/is (= (evaluate (parse "gamma b; max (a) -> max bar") table-bar)
               #{#:R{:max 5, :b "d"}
                 #:R{:max 6, :b "e"}
                 #:R{:max 3, :b "c"}
                 #:R{:max 1, :b "a"}})))
    (t/testing "avg"
      (t/is (= (evaluate (parse "gamma avg (a) -> avg bar") table-bar)
               #:R{:avg 19/5}))
      (t/is (= (evaluate (parse "gamma b; avg (a) -> avg bar") table-bar)
               #{#:R{:avg 6, :b "e"}
                 #:R{:avg 9/2, :b "d"}
                 #:R{:avg 3, :b "c"}
                 #:R{:avg 1, :b "a"}})))
    (t/testing "count"
      (t/is (= (evaluate (parse "gamma count (a) -> count bar") table-bar)
               #:R{:count 5}))
      (t/is (= (evaluate (parse "gamma b; count (a) -> count bar") table-bar)
               #{#:R{:count 1, :b "a"}
                 #:R{:count 1, :b "e"}
                 #:R{:count 1, :b "c"}
                 #:R{:count 2, :b "d"}})))
    (t/testing "count star"
      (t/is (= (evaluate (parse "gamma count (*) -> count bar") table-bar)
               #:R{:count 5}))
      (t/is (= (evaluate (parse "gamma b; count (*) -> count bar") table-bar)
               #{#:R{:count 1, :b "a"}
                 #:R{:count 1, :b "e"}
                 #:R{:count 1, :b "c"}
                 #:R{:count 2, :b "d"}}))))

  (t/testing "union"
    (t/is (= (evaluate (parse "Foo union Foop") (merge table-foo table-foo-prime))
             #{{:R/x 5, :S/x 6.3, :R/y 6} {:R/x 7, :S/x 14, :R/y 12}
               {:R/x 3, :S/x 9.4, :R/y 2}})))

  (t/testing "subtraction"
    (t/is (= (evaluate (parse "Foo except Foop") (merge table-foo table-foo-prime))
             #{{:R/x 3, :S/x 9.4, :R/y 2}})))

  (t/testing "cross product"
    (t/is (= (evaluate (parse "bar * baz") (merge table-bar table-baz))
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
    (t/is (= (evaluate (parse "Foo join (R.x = Q.x) Baz")
                       {"Foo" #{{:R/x 5 :S/x 2.1 :R/y 6} {:R/x 3 :S/x 2.2 :R/y 2} {:R/x 5 :S/x 2.3 :R/y 7}}
                        "Baz" #{{:Q/x 5 :Q/a 1} {:Q/x 3 :Q/a 2}}})
             #{{:R/x 5, :S/x 2.3, :R/y 7, :Q/x 5, :Q/a 1}
               {:R/x 3, :S/x 2.2, :R/y 2, :Q/x 3, :Q/a 2}
               {:R/x 5, :S/x 2.1, :R/y 6, :Q/x 5, :Q/a 1}})))

  (t/testing "division"
    (= (evaluate (parse "(pi R.x, R.y Foo) / Quux")
                 {"Foo" #{{:R/x 5 :S/x 2.1 :R/y 6} {:R/x 3 :S/x 2.2 :R/y 2} {:R/x 5 :S/x 2.3 :R/y 7}}
                  "Bar" #{{:R/x 10 :S/x 2.4 :R/y 9} {:R/x 5 :S/x 2.1 :R/y 6}}
                  "Baz" #{{:Q/x 5 :Q/a 1} {:Q/x 3 :Q/a 2}}
                  "Quux" #{{:R/y 6} {:R/y 7}}})
       #{#:R{:x 5}})))
