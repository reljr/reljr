(ns reljr.file-handler
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn get-table-name
  [filename]
  (first (str/split filename #"\.")))

(def type-coercions {"number" #(java.lang.Float/parseFloat %)})

(defn csv-data->maps [csv-data the-name]
  ;;boilerplate from https://github.com/clojure/data.csv
  (let [raw-cols (first csv-data)
        col-pairs (for [col raw-cols
                        :let [[col-name col-type] (str/split col #":")]]
                    [(keyword the-name col-name)
                     (get type-coercions col-type identity)])
        cols (map first col-pairs)
        coercions (into {} col-pairs)
        raw-data (map zipmap (repeat cols) (rest csv-data))]
    (for [record raw-data]
      (->> (for [[k v] record]
             [k ((coercions k) v)])
           (into {})))))

(defn get-table-data
  ([filename] (get-table-data filename (get-table-name filename)))
  ([filename table-name]
   (if (.exists (io/file filename))
     {table-name
      (set (csv-data->maps (with-open [reader (io/reader filename)]
                             (doall (csv/read-csv reader)))
                           table-name))}
     (println (str "Couldn't find file " filename ".")))))

(defn write-table-data [table filename]
  (when-let [table (seq table)]
    (when-let [cols (seq (keys (first table)))]
      (let [proj (apply juxt cols)
            first-record (first table)
            names (for [col cols]
                    (cond
                      (number? (col first-record)) (str (name col) ":number")
                      :otherwise                   (str (name col) ":string")))]
        (with-open [writer (io/writer filename)]
          (csv/write-csv writer (cons names (map proj table))))))))
