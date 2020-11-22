(ns reljr.file
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn get-table-name
  [filename]
  (first (str/split filename #"\.")))


;;need to map each member of table-headers to be namespaced by table-name, and
;;then map each of those to the values in row


(defn get-table [filename table]
  (let [table-name (get-table-name filename)
        table-headers (first table)
        table-rows (rest table)
        ids (map keyword table-name table-headers)]
    (for [row table-rows]
      ids)))

(get-table "foo.csv"
           (with-open [reader (io/reader (io/resource "foo.csv"))]
             (doall (csv/read-csv reader))))
