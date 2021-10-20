(ns reljr.table-test
  (:require [cljs.test :refer-macros [deftest is]]
            [reljr.table :as table]
            [clojure.string :as s]))

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

(def table-four
  #{{:S/a 1 :S/d 2 :S/g 3}
    {:S/a 1 :S/d 7 :S/g 2}
    {:S/a 2 :S/d 5 :S/g 1}})

(deftest RAInterpreterProjection
  (is (= #{#:R{:a 2}
           #:R{:a 1}}
         (table/project table-one [:R/a])))
  (is (= #{#:R{:d 234}
           #:R{:d 43}
           #:R{:d 3845}}
         (table/project table-two [:R/d])))
  (is (= #{#:Customers{:name "Joanne"}
           #:Customers{:name "Henry"}}
         (table/project table-three [:Customers/name]))))

(deftest RAInterpreterSelection
  (is (= #{#:R{:a 2, :b 5, :c 1}}
         (table/select table-one #(= (:R/a %) 2))))
  (is (= #{#:R{:a 2234, :b 35465, :d 3845}
           #:R{:a 133, :b 7345, :d 234}}
         (table/select table-two #(not= (:R/a %) 154))))
  (is (= #{#:Customers{:name "Henry", :city "Miami", :budget 20000}}
         (table/select table-three #(s/starts-with? (:Customers/name %) "H")))))

(deftest RARenameColumn
  (is (= #{#:R{:b 5, :c 1, :thelettera 2}
           #:R{:b 7, :c 2, :thelettera 1}
           #:R{:b 2, :c 3, :thelettera 1}}
         (table/rename-column table-one :R/a :R/thelettera)))
  (is (= #{#:Customers{:name "Joanne", :city "New York City", :money 80000}
           #:Customers{:name "Henry", :city "Miami", :money 20000}}
         (table/rename-column table-three :Customers/budget :Customers/money))))

(deftest RARenameRelation
  (is (= #{#:NewTable{:a 2234, :b 35465, :d 3845}
           #:NewTable{:a 133, :b 7345, :d 234}
           #:NewTable{:a 154, :b 3452, :d 43}}
         (table/rename table-two "NewTable"))))

(deftest RAOrderBy
  (is (= #{#:R{:a 2, :b 5, :c 1}
           #:R{:a 1, :b 2, :c 3}
           #:R{:a 1, :b 7, :c 2}}
         (table/order-records-by table-one [[:R/c <]])))
  (is (= #{#:Customers{:name "Joanne", :city "New York City", :budget 80000}
           #:Customers{:name "Henry", :city "Miami", :budget 20000}}
         (table/order-records-by table-three [[:Customers/budget >]])))
  (is (= #{#:R{:a 154, :b 3452, :d 43}
           #:R{:a 133, :b 7345, :d 234}
           #:R{:a 2234, :b 35465, :d 3845}}
         (table/order-records-by table-two [[:R/b <]]))))

(deftest RACrossProduct
  (is (= #{#:R{:a 133, :b 7345, :c 3, :d 234} #:R{:a 133, :b 7345, :c 2, :d 234}
           #:R{:a 2234, :b 35465, :c 2, :d 3845} #:R{:a 2234, :b 35465, :c 1, :d 3845}
           #:R{:a 154, :b 3452, :c 3, :d 43} #:R{:a 133, :b 7345, :c 1, :d 234}
           #:R{:a 2234, :b 35465, :c 3, :d 3845} #:R{:a 154, :b 3452, :c 2, :d 43}
           #:R{:a 154, :b 3452, :c 1, :d 43}}
         (table/cross-product table-one table-two)))
  (is (= #{{:R/a 1 :R/b 7 :R/c 2 :Customers/name "Henry" :Customers/city "Miami" :Customers/budget 20000}
           {:R/a 1 :R/b 7 :R/c 2 :Customers/name "Joanne" :Customers/city "New York City" :Customers/budget 80000}
           {:R/a 1 :R/b 2 :R/c 3 :Customers/name "Henry" :Customers/city "Miami" :Customers/budget 20000}
           {:R/a 1 :R/b 2 :R/c 3 :Customers/name "Joanne" :Customers/city "New York City" :Customers/budget 80000}
           {:R/a 2 :R/b 5 :R/c 1 :Customers/name "Joanne" :Customers/city "New York City" :Customers/budget 80000}
           {:R/a 2 :R/b 5 :R/c 1 :Customers/name "Henry" :Customers/city "Miami" :Customers/budget 20000}}
         (table/cross-product table-one table-three))))

(deftest RAInnerJoin
  (is (= #{{:R/a 2, :R/b 5, :R/c 1, :S/a 2, :S/d 5, :S/g 1}
           {:R/a 1, :R/b 2, :R/c 3, :S/a 1, :S/d 7, :S/g 2}
           {:R/a 1, :R/b 7, :R/c 2, :S/a 1, :S/d 7, :S/g 2}
           {:R/a 1, :R/b 2, :R/c 3, :S/a 1, :S/d 2, :S/g 3}
           {:R/a 1, :R/b 7, :R/c 2, :S/a 1, :S/d 2, :S/g 3}}
         (table/inner-join table-one table-four #(= (:R/a %) (:S/a %2))))))
