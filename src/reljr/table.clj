(ns reljr.table
  "Table abstraction for reljr interpreter. All functions assume that all tables
  exist, contain the columns that are being used in the operation, that schemas
  are unifiable, etc. These assumptions are dealt with by the functions in
  interpreter."
  (:require [clojure.set :as set]))

(defn columns-of [table]
  (when-let [[r] (seq table)]
    (into #{} (keys r))))

(defn project [table keys]
  "Project out every value for every row in `table` that matches a key in keys."
  (let [val-funcs (map #(if (vector? %) (first %) %) keys)
        col-names (map #(if (vector? %) (second %) %) keys)
        projection (apply juxt val-funcs)]
    (into #{}
          (map (fn [record]
                 (into {} (map vector col-names (projection record)))))
          table)))

(defn select [table test]
  (into #{}
        (filter test)
        table))

(defn rename [table table-name]
  (letfn [(transform-record [r]
            (into {}
                  (map (fn [[k v]]
                         [(keyword table-name (name k)) v]))
                  r))]
    (into #{}
          (map transform-record)
          table)))

(defn rename-column
  [table from to]
  ;; Assumes from and to are already qualified properly
  (into #{}
        (map (fn [r] (-> r (assoc to (from r)) (dissoc from))))
        table))

(defn order-records-by
  "Order the tuples in table by using the rules in col-rules.

  We consider the 'remaining' cols in addition to the order-by cols because of
  the case where two tuples match on all but one column. Crucially, the unique
  column must not be one of the order-by cols. In this scenario, one of the two
  records will be dropped, which is probably not desired behavior.
  "
  [table col-rules]
  (let [used-cols (into #{} (map first) col-rules)]
    (if-let [first-record (first table)]
      (let [remaining-cols (set/difference (into #{} (keys first-record))
                                           used-cols)
            full-rules (concat col-rules (map (fn [c] [c #(< (compare %1 %2) 0)]) remaining-cols))]
        (letfn [(ordering [a b]
                  ;; a and b are tuples in the table
                  (loop [rules full-rules]
                    (if-let [[[column ord]] (seq rules)]
                      (let [l (column a)
                            r (column b)]
                        ;; apply the actual comparator for this column
                        (cond (ord l r) true
                              (ord r l) false
                              ;; when the two records were equal, we must use the
                              ;; other rules
                              :otherwise (recur (rest rules))))
                      false)))]
          ;; create an empty sorted set whose comparator is the ordering
          ;; function
          (into (sorted-set-by ordering) table)))
      #{})))

(defn group-records-by
  "Return a new table whose columns are those in group-cols and the column in
  aggregation."
  [table group-cols aggregation]
  ;; if we have more than zero columns to group on, group on them
  (if (seq group-cols)
    ;; record-groups is a map whose keys are a vector of the values of the
    ;; columns that we want to extract in group-cols; the values of the map are
    ;; lists of the tuples that contained that value.
    (let [record-groups (group-by (apply juxt group-cols) table)
          grouped-records
          (map (fn [[group records]]
                 (into (aggregation records)
                       (map vector group-cols group)))
               record-groups)]
      (into #{} grouped-records))
    (aggregation table)))

(defn cross-product [table1 table2]
  (into #{} (for [r1 table1
                  r2 table2]
              (into r1 r2))))

(defn inner-join [table1 table2 test]
  (into #{} (for [r1 table1
                  r2 table2
                  :when (test r1 r2)]
              (into r1 r2))))

(defn make-natural-join-test [table1 table2]
  "Return the inner-join condition and the columns that should be projected out
  to accomplish a natural join using project and inner-join."
  (let [cols1 (keys (first table1))
        cols2 (keys (first table2))
        cols (into #{} (concat cols1 cols2))
        grouped-cols1 (group-by name cols1)
        grouped-cols2 (group-by name cols2)
        test-groups (for [[g1 cs1] grouped-cols1
                          [g2 cs2] grouped-cols2
                          :when (= g1 g2)]
                      [[(apply juxt cs1) (apply juxt cs2)]
                       (set cs2)])
        tests (map first test-groups)
        dropped-cols (map second test-groups)
        proj-cols (reduce set/difference cols dropped-cols)]
    ;; left and right records, the argument to the lambda
    [(fn [l r]
       ;; "left juxt" and "right juxt", a test from above that looks like:
       ;; (apply juxt [:S/a])
       (every? (fn [[lj rj]]
                 (every? (set (rj r))
                         (lj l)))
               tests))
     proj-cols]))

(defn natural-join [table1 table2]
  "Perform the natural join by combining projection, inner join, and a custom
  test function."
  (let [[test cols] (make-natural-join-test table1 table2)]
    (project (inner-join table1 table2 test) cols)))
