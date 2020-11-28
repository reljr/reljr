(ns reljr.table
  (:require [clojure.set :as set]))

(def some-table
  #{{:R/a 1 :R/b 2 :R/c 3}
    {:R/a 1 :R/b 7 :R/c 2}
    {:R/a 2 :R/b 5 :R/c 1}})

(defn columns-of [table]
  (when-let [[r] (seq table)]
    (into #{} (keys r))))

(defn project [table keys]
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

(defn order-records-by [table col-rules]
  (let [used-cols (into #{} (map first) col-rules)]
    (if-let [first-record (first table)]
      (let [remaining-cols (set/difference (into #{} (keys first-record))
                                           used-cols)
            full-rules (concat col-rules (map (fn [c] [c <]) remaining-cols))]
        (letfn [(ordering [a b]
                  (loop [rules full-rules]
                    (if-let [[[column ord]] (seq rules)]
                      (let [l (column a)
                            r (column b)]
                        (cond (ord l r) true
                              (ord r l) false
                              :otherwise (recur (rest rules))))
                      false)))]
          (into (sorted-set-by ordering) table)))
      #{})))

(defn group-records-by [table group-cols aggregation]
  (if (seq group-cols)
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
    [(fn [l r]
       (every? (fn [[lj rj]]
                 (every? (set (rj r))
                         (lj l)))
               tests))
     proj-cols]))

(defn natural-join [table1 table2]
  (let [[test cols] (make-natural-join-test table1 table2)]
    (project (inner-join table1 table2 test) cols)))
