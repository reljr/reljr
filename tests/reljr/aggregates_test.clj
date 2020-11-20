(ns reljr.aggregates-test
  (:require  [clojure.test :as t])
  (:require [reljr.aggregates :as a]))

(def some-table
  #{{:R/a 1 :R/b 2 :R/c 3}
    {:R/a 1 :R/b 7 :R/c 2}
    {:R/a 2 :R/b 5 :R/c 1}})

(t/deftest RAAggregateMin
  (t/is (= (a/mincol some-table :R/b)
           2)))

(t/deftest RAAggregateMax
  (t/is (= (a/maxcol some-table :R/c)
           3)))

(t/deftest RAAggregateSum
  (t/is (= (a/sum some-table :R/a) 4)))

(t/deftest RAAggregateAvg
  (t/is (= (a/avg some-table :R/b) 14/3)))
