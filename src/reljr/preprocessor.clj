(ns reljr.preprocessor
  (:require [reljr.table :as table]
            [reljr.aggregates :as agg]
            [reljr.expression-utilities :as eutils]
            [clojure.set :as set]))

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
            new-column (keyword (when (keyword? column) (namespace column)) new-name)]
        new-column))))

(defn preprocess-predicate [boolexp known-cols]
  (case (first boolexp)
    :Column (resolve-column boolexp known-cols)
    :number (read-string (second boolexp))
    :string (second boolexp)
    :NotExpr (let [[_ exp] boolexp
                   pred (preprocess-predicate exp known-cols)]
               ['not pred])
    :AndExpr (let [[_ lexp rexp] boolexp
                   lpred (preprocess-predicate lexp known-cols)
                   rpred (preprocess-predicate rexp known-cols)]
               ['and lpred rpred])
    :OrExpr (let [[_ lexp rexp] boolexp
                  lpred (preprocess-predicate lexp known-cols)
                  rpred (preprocess-predicate rexp known-cols)]
              ['or lpred rpred])
    :EqualsExpr (let [[_ lexp rexp] boolexp
                      lpred (preprocess-predicate lexp known-cols)
                      rpred (preprocess-predicate rexp known-cols)]
                  ['= lpred rpred])
    :NotEqualsExpr (let [[_ lexp rexp] boolexp
                         lpred (preprocess-predicate lexp known-cols)
                         rpred (preprocess-predicate rexp known-cols)]
                     ['not= lpred rpred])
    :GreaterExpr (let [[_ lexp rexp] boolexp
                       lpred (preprocess-predicate lexp known-cols)
                       rpred (preprocess-predicate rexp known-cols)]
                   ['> lpred rpred])
    :GreaterEqualExpr (let [[_ lexp rexp] boolexp
                            lpred (preprocess-predicate lexp known-cols)
                            rpred (preprocess-predicate rexp known-cols)]
                        ['>= lpred rpred])
    :LessExpr (let [[_ lexp rexp] boolexp
                    lpred (preprocess-predicate lexp known-cols)
                    rpred (preprocess-predicate rexp known-cols)]
                ['< lpred rpred])
    :LessEqualExpr (let [[_ lexp rexp] boolexp
                         lpred (preprocess-predicate lexp known-cols)
                         rpred (preprocess-predicate rexp known-cols)]
                     ['<= lpred rpred])
    :MinusExpr (let [[_ lexp rexp] boolexp
                     lpred (preprocess-predicate lexp known-cols)
                     rpred (preprocess-predicate rexp known-cols)]
                 ['- lpred rpred])
    :PlusExpr (let [[_ lexp rexp] boolexp
                    lpred (preprocess-predicate lexp known-cols)
                    rpred (preprocess-predicate rexp known-cols)]
                ['+ lpred rpred])
    :TimesExpr (let [[_ lexp rexp] boolexp
                     lpred (preprocess-predicate lexp known-cols)
                     rpred (preprocess-predicate rexp known-cols)]
                 ['* lpred rpred])
    :DivisionExpr (let [[_ lexp rexp] boolexp
                        lpred (preprocess-predicate lexp known-cols)
                        rpred (preprocess-predicate rexp known-cols)]
                    ['<= lpred rpred])
    :ModExpr (let [[_ lexp rexp] boolexp
                   lpred (preprocess-predicate lexp known-cols)
                   rpred (preprocess-predicate rexp known-cols)]
               ['<= lpred rpred])
    :NegationExpr (let [[_ exp] boolexp
                        pred (preprocess-predicate exp known-cols)]
                    ['- pred])
    :FunctionExpr (let [[_ name & args] boolexp
                        args (map #(preprocess-predicate % known-cols) args)]
                    (apply vector name args))))

(defn preprocess-aggregation [agg cols]
  (for [[label column new-name] agg]
    (if (nil? new-name)
      [(keyword (namespace (first cols)) column) ['count nil]]
      (let [column-expr (preprocess-predicate column cols)
            new-column (keyword (when (keyword? column-expr) (namespace column-expr))
                                new-name)]
        [new-column (case label
                      :AggregateCount ['count nil]
                      :AggregateMin ['min column-expr]
                      :AggregateMax ['max column-expr]
                      :AggregateSum ['sum column-expr]
                      :AggregateAvg ['avg column-expr])]))))

(defn preprocess-query [query relations]
  (-> (cond
        (string? query) (if-let [relation (get relations query)]
                          {:type :relation :relation query
                           ::eutils/columns (table/columns-of relation)}
                          (throw (ex-info (str "No relation named: " query)
                                          {:type :no-table
                                           :name query})))
        (vector? query)
        (case (first query)
          :Projection (let [cols (butlast (rest query))
                            sub (preprocess-query (last query) relations)
                            known-cols (::eutils/columns sub)
                            resolved-cols (for [col cols]
                                            (case (first col)
                                              :ExprColumn [(preprocess-predicate (nth col 1) known-cols)
                                                           (keyword (nth col 2))]
                                              (let [c (resolve-column col known-cols)]
                                                [c c])))]
                        {:type :projection
                         :sub sub
                         :columns resolved-cols})
          :Selection (let [[_ test sub] query
                           sub (preprocess-query sub relations)
                           known-cols (::eutils/columns sub)
                           pred (preprocess-predicate test known-cols)]
                       {:type :selection
                        :sub sub
                        :predicate pred})
          :RenameRelation (let [[_ rel-name sub] query
                                sub (preprocess-query sub relations)
                                known-cols (::eutils/columns sub)]
                            (if (= (count known-cols) (count (into #{} (map name) known-cols)))
                              {:type :rename-relation
                               :sub sub
                               :name rel-name}
                              (throw (ex-info (str "Normalizing column prefixes would produce an ambiguity: "
                                                   known-cols)
                                              {:type :ambiguous-columns
                                               :columns known-cols}))))
          :RenameColumn (let [cols (partition 2 (butlast (rest query)))
                              sub (last query)
                              sub (preprocess-query sub relations)
                              known-cols (::eutils/columns sub)
                              cols (for [[old new] cols]
                                     (let [c (resolve-column old known-cols)]
                                       [c (keyword (namespace c) new)]))
                              cols (into {} cols)]
                          (reduce (fn [sub [old new]]
                                    (eutils/propogate-column-metadata
                                     {:type :rename-column
                                      :sub sub
                                      :old old :new new}))
                                  sub
                                  cols))
          :OrderBy (let [cols (butlast (rest query))
                         sub (last query)
                         sub (preprocess-query sub relations)
                         known-cols (::eutils/columns sub)
                         cols (for [[o c] cols]
                                [(resolve-column c known-cols)
                                 ({:AscendingColumn '<
                                   :DescendingColumn '>}
                                  o)])]
                     {:type :order-by
                      :sub sub
                      :orderings (into [] cols)})
          :GroupBy (let [cols (butlast (rest query))
                         [group-cols agg-cols] (split-with #(= :Column (first %)) cols)
                         sub (last query)
                         sub (preprocess-query sub relations)
                         known-cols (::eutils/columns sub)
                         group-cols (resolve-columns group-cols known-cols)
                         agg (preprocess-aggregation agg-cols known-cols)]
                     {:type :group-by
                      :sub sub
                      :group-columns group-cols
                      :aggregation agg})
          :Union (let [[_ lexp rexp] query
                       lexp (preprocess-query lexp relations)
                       lcols (::eutils/columns lexp)
                       rexp (preprocess-query rexp relations)
                       rcols (::eutils/columns rexp)]
                   (if (= (set lcols) (set rcols))
                     {:type :union
                      :left lexp
                      :right rexp}
                     (throw (ex-info (str "Incompatible Schemas: " lcols " " rcols)
                                     {:type :incompatible-schema
                                      :left lcols
                                      :right rcols}))))
          :Subtraction (let [[_ lexp rexp] query
                             lexp (preprocess-query lexp relations)
                             lcols (::eutils/columns lexp)
                             rexp (preprocess-query rexp relations)
                             rcols (::eutils/columns rexp)]
                         (if (= (set lcols) (set rcols))
                           {:type :subtraction
                            :left lexp
                            :right rexp}
                           (throw (ex-info (str "Incompatible Schemas: " lcols " " rcols)
                                           {:type :incompatible-schema
                                            :left lcols
                                            :right rcols}))))
          :Intersection (let [[_ lexp rexp] query
                              lexp (preprocess-query lexp relations)
                              lcols (::eutils/columns lexp)
                              rexp (preprocess-query rexp relations)
                              rcols (::eutils/columns rexp)]
                          (if (= (set lcols) (set rcols))
                            {:type :intersection
                             :left lexp
                             :right rexp}
                            (throw (ex-info (str "Incompatible Schemas: " lcols " " rcols)
                                            {:type :incompatible-schema
                                             :left lcols
                                             :right rcols}))))
          :Division (let [[_ lexp rexp] query
                          lexp (preprocess-query lexp relations)
                          lcols (::eutils/columns lexp)
                          rexp (preprocess-query rexp relations)
                          rcols (::eutils/columns rexp)
                          group-cols (set/difference (set lcols) (set rcols))]
                      (if (= (set/difference (set rcols) (set lcols)) #{})
                        {:type :division
                         :group-cols group-cols
                         :left lexp
                         :right rexp}
                        (throw (ex-info (str "Incompatible Schemas: " lcols " " rcols)
                                        {:type :incompatible-schema
                                         :left lcols
                                         :right rcols}))))
          :CrossProduct (let [[_ lexp rexp] query
                              lexp (preprocess-query lexp relations)
                              lcols (::eutils/columns lexp)
                              rexp (preprocess-query rexp relations)
                              rcols (::eutils/columns rexp)]
                          (if (= (set/intersection (set lcols) (set rcols)) #{})
                            {:type :cross-product
                             :left lexp
                             :right rexp}
                            (throw (ex-info (str "Incompatible Schemas: " lcols " " rcols)
                                            {:type :incompatible-schema
                                             :left lcols
                                             :right rcols}))))
          :InnerJoin (let [[_ lexp boolexp rexp] query
                           lexp (preprocess-query lexp relations)
                           lcols (::eutils/columns lexp)
                           rexp (preprocess-query rexp relations)
                           rcols (::eutils/columns rexp)
                           known-cols (set/union (set lcols) (set rcols))
                           rel (preprocess-predicate boolexp known-cols)]
                       (if (= (set/intersection (set lcols) (set rcols)) #{})
                         {:type :inner-join
                          :left lexp
                          :right rexp
                          :relation rel}
                         (throw (ex-info (str "Incompatible Schemas: " lcols " " rcols)
                                         {:type :incompatible-schema
                                          :left lcols
                                          :right rcols}))))
          :NaturalJoin (let [[_ lexp rexp] query
                             lexp (preprocess-query lexp relations)
                             lcols (::eutils/columns lexp)
                             l-groups (group-by name lcols)
                             rexp (preprocess-query rexp relations)
                             rcols (::eutils/columns rexp)
                             r-groups (group-by name rcols)
                             dropped-groups (for [[g1 cs1] l-groups
                                                  [g2 cs2] r-groups
                                                  :when (= g1 g2)]
                                              (set cs2))
                             cols (reduce set/difference
                                          (into #{} (concat lcols rcols))
                                          dropped-groups)]
                         {:type :natural-join
                          :left lexp
                          :right rexp})))
      eutils/propogate-column-metadata))
