(ns reljr.aggregates)

(def some-table
  #{{:R/a 1 :R/b 2 :R/c 3}
    {:R/a 1 :R/b 7 :R/c 2}
    {:R/a 2 :R/b 5 :R/c 1}})

(defn min [table column]
  (reduce clojure.core/min (map column table)))

(defn max [table column]
  (reduce clojure.core/max (map column table)))

(defn sum [table column]
  (reduce + (map column table)))

(defn cnt [table column]
  (reduce clojure.core/count table))

(defn avg [table column]

  (/ (sum table column)
     (clojure.core/count table)))

(sum some-table :R/c)

(avg some-table :R/b)

(cnt some-table :R/b)
