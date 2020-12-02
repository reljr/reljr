(ns reljr.interpreter
  (:require [reljr.table :as table]
            [clojure.set :as set]
            [reljr.aggregates :as agg]))

(defn resolve-column [col known-cols]
  (if (= (count col) 3)
    (keyword (get col 1) (get col 2))
    (first (filter #(= (get col 1) (name %)) known-cols))))
(def predicate-fns
  {'not not
   'and #(and %1 %2)
   'or #(or %1 %2)
   '= =
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

(defn resolve-columns [raw-cols known-cols]
  (into [] (for [col raw-cols]
             (resolve-column col known-cols))))

(defn aggregation-for [agg cols]
  (fn [t]
    (into {}
          (for [[label column new-name] agg]
            (if (nil? new-name)
              [(keyword (namespace (first cols)) column) (count t)]
              (let [column (resolve-column column cols)
                    new-column (keyword (namespace column) new-name)]
                [new-column (case label
                              :AggregateCount (count t)
                              :AggregateMin (agg/mincol t column)
                              :AggregateMax (agg/maxcol t column)
                              :AggregateSum (agg/sum t column)
                              :AggregateAvg (agg/avg t column))]))))))
(defn predicate-runner [predicate]
  (cond
    (keyword? predicate) (fn [& colls] (some predicate colls))
    (number? predicate) (fn [& _] predicate)
    (vector? predicate)
    (let [[func & args] predicate
          func (predicate-fns func)
          args (apply juxt (map predicate-runner args))]
      (fn [& records] (apply func (apply args records))))))

(defn predicate-for [boolexpr known-cols]
  (case (first boolexpr)
    :Column (resolve-column boolexpr known-cols)
    :number (fn [_] (read-string (second boolexpr)))
    :NotExpr (let [[_ exp] boolexpr
                   pred (predicate-for exp known-cols)]
               #(not (pred %)))
    :AndExpr (let [[_ lexp rexp] boolexpr
                   lpred (predicate-for lexp known-cols)
                   rpred (predicate-for rexp known-cols)]
               #(and (lpred %) (rpred %)))
    :OrExpr (let [[_ lexp rexp] boolexpr
                  lpred (predicate-for lexp known-cols)
                  rpred (predicate-for rexp known-cols)]
              #(or (lpred %) (rpred %)))
    :EqualsExpr (let [[_ lexp rexp] boolexpr
                      lpred (predicate-for lexp known-cols)
                      rpred (predicate-for rexp known-cols)]
                  #(= (lpred %) (rpred %)))
    :GreaterExpr (let [[_ lexp rexp] boolexpr
                       lpred (predicate-for lexp known-cols)
                       rpred (predicate-for rexp known-cols)]
                   #(> (lpred %) (rpred %)))
    :GreaterEqualExpr (let [[_ lexp rexp] boolexpr
                            lpred (predicate-for lexp known-cols)
                            rpred (predicate-for rexp known-cols)]
                        #(>= (lpred %) (rpred %)))
    :LessExpr (let [[_ lexp rexp] boolexpr
                    lpred (predicate-for lexp known-cols)
                    rpred (predicate-for rexp known-cols)]
                #(< (lpred %) (rpred %)))
    :LessEqualExpr (let [[_ lexp rexp] boolexpr
                         lpred (predicate-for lexp known-cols)
                         rpred (predicate-for rexp known-cols)]
                     #(<= (lpred %) (rpred %)))))
(def aggregate-fns
  {'count agg/cntcol
   'min agg/mincol
   'max agg/maxcol
   'sum agg/sum
   'avg agg/avg})

(defn relation-for [boolexpr known-cols]
  (case (first boolexpr)
    :Column (let [c (resolve-column boolexpr known-cols)]
              #(or (c %1) (c %2)))
    :number (fn [_ _] (read-string (second boolexpr)))
    :NotExpr (let [[_ exp] boolexpr
                   pred (relation-for exp known-cols)]
               #(not (pred %1 %2)))
    :AndExpr (let [[_ lexp rexp] boolexpr
                   lpred (relation-for lexp known-cols)
                   rpred (relation-for rexp known-cols)]
               #(and (lpred %1 %2) (rpred %1 %2)))
    :OrExpr (let [[_ lexp rexp] boolexpr
                  lpred (relation-for lexp known-cols)
                  rpred (relation-for rexp known-cols)]
              #(or (lpred %1 %2) (rpred %1 %2)))
    :EqualsExpr (let [[_ lexp rexp] boolexpr
                      lpred (relation-for lexp known-cols)
                      rpred (relation-for rexp known-cols)]
                  #(= (lpred %1 %2) (rpred %1 %2)))
    :GreaterExpr (let [[_ lexp rexp] boolexpr
                       lpred (relation-for lexp known-cols)
                       rpred (relation-for rexp known-cols)]
                   #(> (lpred %1 %2) (rpred %1 %2)))
    :GreaterEqualExpr (let [[_ lexp rexp] boolexpr
                            lpred (relation-for lexp known-cols)
                            rpred (relation-for rexp known-cols)]
                        #(>= (lpred %1 %2) (rpred %1 %2)))
    :LessExpr (let [[_ lexp rexp] boolexpr
                    lpred (relation-for lexp known-cols)
                    rpred (relation-for rexp known-cols)]
                #(< (lpred %1 %2) (rpred %1 %2)))
    :LessEqualExpr (let [[_ lexp rexp] boolexpr
                         lpred (relation-for lexp known-cols)
                         rpred (relation-for rexp known-cols)]
                     #(<= (lpred %1 %2) (rpred %1 %2)))))
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
