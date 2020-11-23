(ns reljr.file-handler
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn get-table-name
  [filename]
  (first (str/split filename #"\.")))

(defn csv-data->maps [csv-data the-name]
  ;;boilerplate from https://github.com/clojure/data.csv
  (map zipmap
       (->> (map #(keyword the-name %) (first csv-data))
            (map keyword)
            repeat)
       (rest csv-data)))

(defn get-table-data
  [filename]
  (let [table-name (get-table-name filename)]
    {table-name
     (set (csv-data->maps (with-open [reader (io/reader (io/resource filename))]
                            (doall (csv/read-csv reader))) table-name))}))