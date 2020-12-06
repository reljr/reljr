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
  (cond
    (string? expression) (get relations expression)
    (vector? expression)
    (case (first expression)
      :projection (let [[_ subexpr cols] expression
                        subeval (evaluate subexpr relations)
                        cols (map (fn [[c n]] [(predicate-runner c) n]) cols)]
                    (table/project subeval cols))
      :selection (let [[_ subexpr pred] expression
                       subeval (evaluate subexpr relations)]
                   (table/select subeval (predicate-runner pred)))
      :rename-relation (let [[_ subexpr name] expression
                             subeval (evaluate subexpr relations)]
                         (table/rename subeval name))
      :rename-column (let [[_ subexpr old new] expression
                           subeval (evaluate subexpr relations)]
                       (table/rename-column subeval old new))
      :order-by (let [[_ subexpr cols] expression
                      subeval (evaluate subexpr relations)]
                  (table/order-records-by subeval cols))
      :group-by (let [[_ subexpr group-cols agg] expression
                      subeval (evaluate subexpr relations)
                      agg (aggregation-runner agg)]
                  (table/group-records-by subeval group-cols agg))
      :union (let [[_ lexp rexp] expression
                   lval (evaluate lexp relations)
                   rval (evaluate rexp relations)]
               (set/union lval rval))
      :subtraction (let [[_ lexp rexp] expression
                         lval (evaluate lexp relations)
                         rval (evaluate rexp relations)]
                     (set/difference lval rval))
      :intersection (let [[_ lexp rexp] expression
                          lval (evaluate lexp relations)
                          rval (evaluate rexp relations)]
                      (set/intersection lval rval))
      :division (let [[_ group-cols lexp rexp] expression
                      lval (evaluate lexp relations)
                      rval (evaluate rexp relations)
                      rcols (table/columns-of rval)]
                  (table/project
                   (table/select
                    (table/group-records-by
                     lval group-cols
                     (fn [g] {'keep (= rval (table/project g rcols))}))
                    #(% 'keep))
                   group-cols))
      :cross-product (let [[_ lexp rexp] expression
                           lval (evaluate lexp relations)
                           rval (evaluate rexp relations)]
                       (table/cross-product lval rval))
      :inner-join (let [[_ lexp rexp rel] expression
                        lval (evaluate lexp relations)
                        rval (evaluate rexp relations)]
                    (table/inner-join lval rval (predicate-runner rel)))
      :natural-join (let [[_ lexp rexp] expression
                          lval (evaluate lexp relations)
                          rval (evaluate rexp relations)]
                      (table/natural-join lval rval)))))
