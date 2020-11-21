(ns reljr.interpreter
  (:require [reljr.table :as table]
            [clojure.set :as set]
            [reljr.aggregates :as agg]))

(defn resolve-column [col known-cols]
  (if (= (count col) 3)
    (keyword (get col 1) (get col 2))
    (first (filter #(= (get col 1) (name %)) known-cols))))

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

(defn evaluate [expression relations]
  (cond
    (string? expression) (get relations expression)
    (vector? expression)
    (case (first expression)
      :Projection (let [cols (butlast (rest expression))
                        subexpr (last expression)
                        subeval (evaluate subexpr relations)
                        resolved-cols (resolve-columns cols (table/columns-of subeval))]
                    (table/project subeval resolved-cols))
      :Selection (let [[_ test subexpr] expression
                       subeval (evaluate subexpr relations)
                       pred (predicate-for test (table/columns-of subeval))]
                   (table/select subeval pred))
      :RenameRelation (let [[_ name subexpr] expression
                            subeval (evaluate subexpr relations)]
                        (table/rename subeval name))
      :RenameColumn (let [cols (partition 2 (butlast (rest expression)))
                          subexpr (last expression)
                          subeval (evaluate subexpr relations)
                          known-cols (table/columns-of subeval)
                          cols (for [[old new] cols]
                                 (let [c (resolve-column old known-cols)]
                                   [c (keyword (namespace c) new)]))]
                      (reduce (fn [table [old new]]
                                (table/rename-column table old new))
                              subeval
                              cols))
      :OrderBy (let [cols (butlast (rest expression))
                     subexpr (last expression)
                     subeval (evaluate subexpr relations)
                     known-cols (table/columns-of subeval)
                     cols (for [[o c] cols]
                            [(resolve-column c known-cols)
                             ({:AscendingColumn #(< (compare %1 %2) 0)
                               :DescendingColumn #(> (compare %1 %2) 0)}
                              o)])]
                 (table/order-records-by subeval (into [] cols)))
      :GroupBy (let [cols (butlast (rest expression))
                     [group-cols agg-cols] (split-with #(= :Column (first %)) cols)
                     subexpr (last expression)
                     subeval (evaluate subexpr relations)
                     known-cols (table/columns-of subeval)
                     group-cols (resolve-columns group-cols known-cols)
                     agg (aggregation-for agg-cols known-cols)]
                 (table/group-records-by subeval group-cols agg))
      :Union (let [[_ lexp rexp] expression
                   lval (evaluate lexp relations)
                   rval (evaluate rexp relations)]
               (set/union lval rval))
      :Subtraction (let [[_ lexp rexp] expression
                         lval (evaluate lexp relations)
                         rval (evaluate rexp relations)]
                     (set/difference lval rval))
      :Intersection (let [[_ lexp rexp] expression
                          lval (evaluate lexp relations)
                          rval (evaluate rexp relations)]
                      (set/intersection lval rval))
      :Division (let [[_ lexp rexp] expression
                      lval (evaluate lexp relations)
                      rval (evaluate rexp relations)
                      lcols (table/columns-of lval)
                      rcols (table/columns-of rval)
                      group-cols (set/difference lcols rcols)]
                  (table/project
                   (table/select
                    (table/group-records-by
                     lval group-cols
                     (fn [g] {'keep (= rval (table/project g rcols))}))
                    #(% 'keep))
                   group-cols))
      :CrossProduct (let [[_ lexp rexp] expression
                          lval (evaluate lexp relations)
                          rval (evaluate rexp relations)]
                      (table/cross-product lval rval))
      :InnerJoin (let [[_ lexp boolexpr rexp] expression
                       lval (evaluate lexp relations)
                       rval (evaluate rexp relations)
                       known-cols (set/union (table/columns-of lval)
                                             (table/columns-of rval))
                       rel (relation-for boolexpr known-cols)]
                   (table/inner-join lval rval rel)))))
