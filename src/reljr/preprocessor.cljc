(ns reljr.preprocessor
  (:require [reljr.table :as table]
            [reljr.aggregates :as agg]
            [clojure.set :as set]
            [cljs.reader :as s]))

(defn resolve-column [col known-cols]
  (if (= (count col) 3)
    (keyword (get col 1) (get col 2))
    (or (first (filter #(= (get col 1) (name %)) known-cols))
        (throw (ex-info (str "No such column: " (get col 1))
                        {:type :no-column
                         :name (get col 1)})))))

(defn resolve-columns [raw-cols known-cols]
  (into [] (for [col raw-cols]
             (resolve-column col known-cols))))

(defn aggregation-columns [agg known-cols]
  (for [[label column new-name] agg]
    (if (nil? new-name)
      (keyword (namespace (first known-cols)) column)
      (let [column (resolve-column column known-cols)
            new-column (keyword (namespace column) new-name)]
        new-column))))

(defn preprocess-predicate [boolexpr known-cols]
  (case (first boolexpr)
    :Column (resolve-column boolexpr known-cols)
    :number (s/read-string (second boolexpr))
    :NotExpr (let [[_ exp] boolexpr
                   pred (preprocess-predicate exp known-cols)]
               ['not pred])
    :AndExpr (let [[_ lexp rexp] boolexpr
                   lpred (preprocess-predicate lexp known-cols)
                   rpred (preprocess-predicate rexp known-cols)]
               ['and lpred rpred])
    :OrExpr (let [[_ lexp rexp] boolexpr
                  lpred (preprocess-predicate lexp known-cols)
                  rpred (preprocess-predicate rexp known-cols)]
              ['or lpred rpred])
    :EqualsExpr (let [[_ lexp rexp] boolexpr
                      lpred (preprocess-predicate lexp known-cols)
                      rpred (preprocess-predicate rexp known-cols)]
                  ['= lpred rpred])
    :GreaterExpr (let [[_ lexp rexp] boolexpr
                       lpred (preprocess-predicate lexp known-cols)
                       rpred (preprocess-predicate rexp known-cols)]
                   ['> lpred rpred])
    :GreaterEqualExpr (let [[_ lexp rexp] boolexpr
                            lpred (preprocess-predicate lexp known-cols)
                            rpred (preprocess-predicate rexp known-cols)]
                        ['>= lpred rpred])
    :LessExpr (let [[_ lexp rexp] boolexpr
                    lpred (preprocess-predicate lexp known-cols)
                    rpred (preprocess-predicate rexp known-cols)]
                ['< lpred rpred])
    :LessEqualExpr (let [[_ lexp rexp] boolexpr
                         lpred (preprocess-predicate lexp known-cols)
                         rpred (preprocess-predicate rexp known-cols)]
                     ['<= lpred rpred])
    :MinusExpr (let [[_ lexp rexp] boolexpr
                     lpred (preprocess-predicate lexp known-cols)
                     rpred (preprocess-predicate rexp known-cols)]
                 ['- lpred rpred])
    :PlusExpr (let [[_ lexp rexp] boolexpr
                    lpred (preprocess-predicate lexp known-cols)
                    rpred (preprocess-predicate rexp known-cols)]
                ['+ lpred rpred])
    :TimesExpr (let [[_ lexp rexp] boolexpr
                     lpred (preprocess-predicate lexp known-cols)
                     rpred (preprocess-predicate rexp known-cols)]
                 ['* lpred rpred])
    :DivisionExpr (let [[_ lexp rexp] boolexpr
                        lpred (preprocess-predicate lexp known-cols)
                        rpred (preprocess-predicate rexp known-cols)]
                    ['<= lpred rpred])
    :ModExpr (let [[_ lexp rexp] boolexpr
                   lpred (preprocess-predicate lexp known-cols)
                   rpred (preprocess-predicate rexp known-cols)]
               ['<= lpred rpred])
    :NegationExpr (let [[_ exp] boolexpr
                        pred (preprocess-predicate exp known-cols)]
                    ['not pred])
    :FunctionExpr (let [[_ name & args] boolexpr
                        args (map #(preprocess-predicate % known-cols) args)]
                    (apply vector name args))))

(defn preprocess-aggregation [agg cols]
  (for [[label column new-name] agg]
    (if (nil? new-name)
      [(keyword (namespace (first cols)) column) ['count nil]]
      (let [column (preprocess-predicate column cols)
            new-column (keyword (namespace column) new-name)]
        [new-column (case label
                      :AggregateCount ['count nil]
                      :AggregateMin ['min column]
                      :AggregateMax ['max column]
                      :AggregateSum ['sum column]
                      :AggregateAvg ['avg column])]))))

(defn preprocess-query [query relations]
  (cond
    (string? query) (if-let [relation (get relations query)]
                      [query (table/columns-of relation)]
                      (throw (ex-info (str "No relation named: " query)
                                      {:type :no-table
                                       :name query})))
    (vector? query)
    (case (first query)
      :Projection (let [cols (butlast (rest query))
                        [subexpr known-cols] (preprocess-query (last query) relations)
                        resolved-cols (for [col cols]
                                        (case (first col)
                                          :ExprColumn [(preprocess-predicate (nth col 1) known-cols)
                                                       (keyword (nth col 2))]
                                          (let [c (resolve-column col known-cols)]
                                            [c c])))]
                    [[:projection subexpr resolved-cols]
                     (into [] (map second) resolved-cols)])
      :Selection (let [[_ test subexpr] query
                       [subexpr known-cols] (preprocess-query subexpr relations)
                       pred (preprocess-predicate test known-cols)]
                   [[:selection subexpr pred] known-cols])
      :RenameRelation (let [[_ rel-name subexpr] query
                            [subexpr known-cols] (preprocess-query subexpr relations)]
                        [[:rename-relation subexpr rel-name]
                         (mapv #(keyword rel-name (name %)) known-cols)])
      :RenameColumn (let [cols (partition 2 (butlast (rest query)))
                          subexpr (last query)
                          [subexpr known-cols] (preprocess-query subexpr relations)
                          cols (for [[old new] cols]
                                 (let [c (resolve-column old known-cols)]
                                   [c (keyword (namespace c) new)]))
                          cols (into {} cols)]
                      [(reduce (fn [subexpr [old new]]
                                 [:rename-column subexpr old new])
                               subexpr
                               cols)
                       (mapv #(get cols % %) known-cols)])
      :OrderBy (let [cols (butlast (rest query))
                     subexpr (last query)
                     [subexpr known-cols] (preprocess-query subexpr relations)
                     cols (for [[o c] cols]
                            [(resolve-column c known-cols)
                             ({:AscendingColumn #(< (compare %1 %2) 0)
                               :DescendingColumn #(> (compare %1 %2) 0)}
                              o)])]
                 [[:order-by subexpr (into [] cols)]
                  known-cols])
      :GroupBy (let [cols (butlast (rest query))
                     [group-cols agg-cols] (split-with #(= :Column (first %)) cols)
                     subexpr (last query)
                     [subexpr known-cols] (preprocess-query subexpr relations)
                     group-cols (resolve-columns group-cols known-cols)
                     agg (preprocess-aggregation agg-cols known-cols)]
                 [[:group-by subexpr group-cols agg]
                  (into [] (aggregation-columns agg-cols known-cols))])
      :Union (let [[_ lexp rexp] query
                   [lexp lcols] (preprocess-query lexp relations)
                   [rexp rcols] (preprocess-query rexp relations)]
               (if (= (set lcols) (set rcols))
                 [[:union lexp rexp] lcols]
                 (throw (ex-info (str "Incompatible Schemas: " lcols " " rcols)
                                 {:type :incompatible-schema
                                  :left lcols
                                  :right rcols}))))
      :Subtraction (let [[_ lexp rexp] query
                         [lexp lcols] (preprocess-query lexp relations)
                         [rexp rcols] (preprocess-query rexp relations)]
                     (if (= (set lcols) (set rcols))
                       [[:subtraction lexp rexp] lcols]
                       (throw (ex-info (str "Incompatible Schemas: " lcols " " rcols)
                                       {:type :incompatible-schema
                                        :left lcols
                                        :right rcols}))))
      :Intersection (let [[_ lexp rexp] query
                          [lexp lcols] (preprocess-query lexp relations)
                          [rexp rcols] (preprocess-query rexp relations)]
                      (if (= (set lcols) (set rcols))
                        [[:intersection lexp rexp] lcols]
                        (throw (ex-info (str "Incompatible Schemas: " lcols " " rcols)
                                        {:type :incompatible-schema
                                         :left lcols
                                         :right rcols}))))
      :Division (let [[_ lexp rexp] query
                      [lexp lcols] (preprocess-query lexp relations)
                      [rexp rcols] (preprocess-query rexp relations)
                      group-cols (set/difference (set lcols) (set rcols))]
                  (if (= (set/difference (set rcols) (set lcols)) #{})
                    [[:division group-cols lexp rexp] (vec group-cols)]
                    (throw (ex-info (str "Incompatible Schemas: " lcols " " rcols)
                                    {:type :incompatible-schema
                                     :left lcols
                                     :right rcols}))))
      :CrossProduct (let [[_ lexp rexp] query
                          [lexp lcols] (preprocess-query lexp relations)
                          [rexp rcols] (preprocess-query rexp relations)]
                      (if (= (set/intersection (set lcols) (set rcols)) #{})
                        [[:cross-product lexp rexp] (into lcols rcols)]
                        (throw (ex-info (str "Incompatible Schemas: " lcols " " rcols)
                                        {:type :incompatible-schema
                                         :left lcols
                                         :right rcols}))))
      :InnerJoin (let [[_ lexp boolexpr rexp] query
                       [lexp lcols] (preprocess-query lexp relations)
                       [rexp rcols] (preprocess-query rexp relations)
                       known-cols (set/union (set lcols) (set rcols))
                       rel (preprocess-predicate boolexpr known-cols)]
                   (if (= (set/intersection (set lcols) (set rcols)) #{})
                     [[:inner-join lexp rexp rel] (into lcols rcols)]
                     (throw (ex-info (str "Incompatible Schemas: " lcols " " rcols)
                                     {:type :incompatible-schema
                                      :left lcols
                                      :right rcols}))))
      :NaturalJoin (let [[_ lexp rexp] query
                         [lexp lcols] (preprocess-query lexp relations)
                         l-groups (group-by name lcols)
                         [rexp rcols] (preprocess-query rexp relations)
                         r-groups (group-by name rcols)
                         dropped-groups (for [[g1 cs1] l-groups
                                              [g2 cs2] r-groups
                                              :when (= g1 g2)]
                                          (set cs2))
                         cols (reduce set/difference
                                      (into #{} (concat lcols rcols))
                                      dropped-groups)]
                     [[:natural-join lexp rexp]
                      (vec cols)]))))
