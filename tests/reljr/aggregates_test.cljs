(ns reljr.aggregates-test
  (:require  [cljs.test :refer-macros [deftest is]]
             [reljr.aggregates :as a]))

(def some-table
  #{{:R/a 1 :R/b 2 :R/c 3}
    {:R/a 1 :R/b 7 :R/c 2}
    {:R/a 2 :R/b 5 :R/c 1}})

(deftest RAAggregateMin
  (is (= 2 (a/mincol some-table :R/b))))

(deftest RAAggregateMax
  (is (= 3 (a/maxcol some-table :R/c))))

(deftest RAAggregateSum
  (is (= 4 (a/sum some-table :R/a))))

(deftest RAAggregateAvg
  (is (= (/ 14 3) (a/avg some-table :R/b))))
