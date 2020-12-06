(ns reljr.aggregates-test
  (:require  [cljs.test :refer-macros [deftest is testing run-tests]]
             [reljr.aggregates :as a]))

(def some-table
  #{{:R/a 1 :R/b 2 :R/c 3}
    {:R/a 1 :R/b 7 :R/c 2}
    {:R/a 2 :R/b 5 :R/c 1}})

(deftest RAAggregateMin
  (is (= (a/mincol some-table :R/b)
         2)))

(deftest RAAggregateMax
  (is (= (a/maxcol some-table :R/c)
         3)))

(deftest RAAggregateSum
  (is (= (a/sum some-table :R/a) 4)))

(deftest RAAggregateAvg
  (is (= (a/avg some-table :R/b) (/ 14 3))))
