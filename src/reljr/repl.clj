(ns reljr.repl
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [reljr.file-handler :as files]
            [reljr.parser :as parser]
            [reljr.interpreter :as interp]
            [reljr.preprocessor :as rpp])
  (:gen-class))

(defn print-tables [tables]
  (let [preformatted-tables
        (for [[table contents] tables]
          [table (when-let [contents (seq contents)]
                   (when-let [cols (seq (keys (first contents)))]
                     (for [col cols]
                       (if (namespace col)
                         (str (namespace col) "." (name col))
                         (name col)))))])]
    (pp/cl-format true "~:{~&~A => ~<(~;~@{~A~^, ~:_~}~;)~:>~%~}" preformatted-tables)))

(defn -main []
  (binding [*out* (pp/get-pretty-writer *out*)]
    (loop [tables {}]
      (pp/cl-format true "~&reljr> ")
      (flush)
      (let [input (parser/full-relational-algebra-parser (read-line))]
        (if (insta/failure? input)
          (do (pp/cl-format true "~&~A~%" input)
              (recur tables))
          (let [input (first input)]
            (case (first input)
              :ReadCommand (recur (merge tables (files/get-table-data (second input))))
              :ReadAsCommand (recur (merge tables (files/get-table-data (nth input 1)
                                                                        (nth input 2))))
              :StoreAsCommand (recur (assoc tables
                                            (nth input 2)
                                            (interp/evaluate (nth input 1) tables)))
              :RenameCommand (recur (dissoc (assoc tables
                                                   (nth input 2)
                                                   (get tables (nth input 1)))
                                            (nth input 1)))
              :DeleteCommand (recur (dissoc tables (nth input 1)))
              :WriteCommand (do (files/write-table-data (get tables (nth input 1))
                                                        (str (nth input 1) ".csv"))
                                (recur tables))
              :WriteAsCommand (do (files/write-table-data (get tables (nth input 1))
                                                          (nth input 2))
                                  (recur tables))
              :ListCommand (do (print-tables tables)
                               (recur tables))
              :QuitCommand ()
              :QueryCommand (do (pp/print-table (interp/evaluate (nth input 1) tables))
                                (recur tables)))))))))
