(ns reljr.aggregates)

(defn mincol [table column]
  (reduce clojure.core/min (map column table)))

(defn maxcol [table column]
  (reduce clojure.core/max (map column table)))

(defn sum [table column]
  (reduce + (map column table)))

(defn cntcol [table column]
  (reduce clojure.core/count table))

(defn avg [table column]

  (/ (sum table column)
     (clojure.core/count table)))
