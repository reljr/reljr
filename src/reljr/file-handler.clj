(ns reljr.file-handler
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn get-table-name
  [filename]
  (first (str/split filename #"\.")))

(defn csv-data->maps [csv-data]
  ;;boilerplate from https://github.com/clojure/data.csv
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map keyword) ;; Drop if you want string keys instead
            repeat)
       (rest csv-data)))

(defn get-table-data
  [filename]
  {(get-table-name filename)
   (csv-data->maps (with-open [reader (io/reader (io/resource filename))]
                     (doall (csv/read-csv reader))))})
