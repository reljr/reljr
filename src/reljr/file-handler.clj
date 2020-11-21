(ns reljr.file
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn get-table-name [filename]
  (first (str/split filename #"\.")))

(defn get-file [filename]
  {(get-table-name filename)})

(with-open [reader (io/resource "foo.csv")]
  (doall (csv/read-csv reader)))
