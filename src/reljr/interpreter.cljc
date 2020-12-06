(ns reljr.interpreter
  (:require [reljr.table :as table]
            [clojure.set :as set]
            [reljr.aggregates :as agg]
            [reljr.expression-utilities :as eutils]))

(def predicate-fns
  {'not not
   'and (fn [& vs] (every? identity vs))
   'or (fn [& vs] (some identity vs))
   '= =
   'not= not=
   '> >
   '>= >=
   '< <
   '<= <=
   '+ +
   '- -
   '* *
   '/ /
   '% mod
   "length" count})

(defn predicate-runner
  "Convert a predicate expression into a predicate function, which is then called
  with one or more records later."
  [predicate]
  (cond
    (keyword? predicate) (fn [& colls] (some predicate colls))
    (or (string? predicate) (number? predicate)) (fn [& _] predicate)
    (vector? predicate)
    (let [[func & args] predicate
          func (predicate-fns func)
          args (apply juxt (map predicate-runner args))]
      (fn [& records] (apply func (apply args records))))))

(def aggregate-fns
  {'count agg/cntcol
   'min agg/mincol
   'max agg/maxcol
   'sum agg/sum
   'avg agg/avg})

(defn aggregation-runner [aggregation]
  (let [aggregation
        (for [[col [op arg]] aggregation]
          [col [(aggregate-fns op) (predicate-runner arg)]])]
    (fn [records]
      (into {}
            (for [[col [op arg]] aggregation]
              [col (op records arg)])))))

(defn evaluate
  "Evaluates a relational algebra expression over any of the available relations."
  [expression relations]
  (letfn [(postwalker [expression]
            (case (:type expression)
              :relation (get relations (:relation expression))
              :projection (table/project (:sub expression)
                                         (map (fn [[c n]]
                                                [(predicate-runner c) n])
                                              (:columns expression)))
              :selection (table/select (:sub expression)
                                       (predicate-runner (:predicate expression)))
              :rename-relation (table/rename (:sub expression) (:name expression))
              :rename-column (table/rename-column (:sub expression)
                                                  (:old expression)
                                                  (:new expression))
              :order-by (table/order-records-by (:sub expression)
                                                (map (fn [[c n]] [c ({'< #(< (compare %1 %2) 0)
                                                                      '> #(> (compare %1 %2) 0)} n)])
                                                     (:orderings expression)))
              :group-by (table/group-records-by (:sub expression)
                                                (:group-columns expression)
                                                (aggregation-runner
                                                 (:aggregation expression)))
              :union (set/union (:left expression) (:right expression))
              :subtraction (set/difference (:left expression) (:right expression))
              :intersection (set/intersection (:left expression) (:right expression))
              :division (let [{:keys [group-cols left right]} expression
                              rcols (table/columns-of right)]
                          (table/project
                           (table/select
                            (table/group-records-by
                             left group-cols
                             (fn [g] {'keep (= right (table/project g rcols))}))
                            #(% 'keep))
                           group-cols))
              :cross-product (table/cross-product (:left expression)
                                                  (:right expression))
              :inner-join (table/inner-join (:left expression)
                                            (:right expression)
                                            (predicate-runner
                                             (:relation expression)))
              :natural-join (table/natural-join (:left expression)
                                                (:right expression))))]
    (eutils/raexpression-walker expression identity postwalker)))
