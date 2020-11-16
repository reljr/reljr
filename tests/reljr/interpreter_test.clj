(ns reljr.interpreter-test
  (:require  [clojure.test :as t])
  (:require [reljr.interpreter :as i]))

(def table-one
  #{{:R/a 1 :R/b 2 :R/c 3}
    {:R/a 1 :R/b 7 :R/c 2}
    {:R/a 2 :R/b 5 :R/c 1}})

(def table-two
  #{{:R/a 154 :R/b 3452 :R/d 43}
    {:R/a 133 :R/b 7345 :R/d 234}
    {:R/a 2234 :R/b 35465 :R/d 3845}})

(def table-three
  #{{:Customers/name "Henry" :Customers/city "Miami" :Customers/budget 20000}
    {:Customers/name "Joanne" :Customers/city "New York City" :Customers/budget 80000}})

(i/project table-three [:Customers/name])

(t/deftest RAInterpreterProjection
  (t/is (= (i/project table-one [:R/a]) #{#:R{:a 2} #:R{:a 1}}))
  (t/is (= (i/project table-two [:R/d]) #{#:R{:d 234} #:R{:d 43} #:R{:d 3845}}))
  (t/is (= (i/project table-three [:Customers/name]) #{#:Customers{:name "Joanne"} #:Customers{:name "Henry"}})))
