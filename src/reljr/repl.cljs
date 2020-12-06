(ns reljr.repl
  (:require [instaparse.core :as insta]
            [reljr.file-handler :as file]
            [reljr.frontend-state :as fstate]
            [reljr.interpreter :as interp]
            [reljr.parser :as parser]
            [reljr.preprocessor :as rpp]
            [reljr.optimizer :as opti]
            [reljr.expression-utilities :as eutils]
            [cljs.pprint :as pp]))

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

(defn web-main []
  (let [input (parser/full-relational-algebra-parser @fstate/current-line)]
    (if (insta/failure? input)
      (reset! fstate/main-result
              (with-out-str (pp/cl-format true "~&~A~%" input)))
      (let [input (first input)
            tables @fstate/all-tables]
        (case (first input)
          :ReadCommand (file/csv-dialog)
          :StoreAsCommand (try
                            (swap! fstate/all-tables merge
                                   (assoc tables
                                          (nth input 2)
                                          (interp/evaluate
                                           (opti/optimize
                                            (rpp/preprocess-query
                                             [:RenameRelation
                                              (nth input 2)
                                              (nth input 1)]
                                             tables)
                                            tables)
                                           tables)))
                            (catch js/Error e
                              (reset! fstate/main-result (ex-message e))))
          :RenameCommand (reset! fstate/all-tables
                                 (dissoc (assoc tables
                                                (nth input 2)
                                                (get tables (nth input 1)))
                                         (nth input 1)))
          :DeleteCommand (reset! fstate/all-tables (dissoc tables (nth input 1)))
          :WriteCommand (file/download-blob (str (nth input 1) ".csv") (get tables (nth input 1)))

          :WriteAsCommand (file/download-blob (str (nth input 2) ".csv") (get tables (nth input 1)))
          :ListCommand (reset! fstate/main-result
                               (with-out-str (print-tables tables)))
          :QuitCommand (;;TODO
                        )
          :PreprocessCommand (reset! fstate/main-result
                                     (with-out-str
                                       (try
                                         (eutils/pprint-raexpression
                                          (rpp/preprocess-query (nth input 1) tables))
                                         (catch js/Error e
                                           (println (str (ex-message e)))))))
          :OptimizeCommand (reset! fstate/main-result
                                   (with-out-str
                                     (try
                                       (eutils/pprint-raexpression
                                        (opti/optimize
                                         (rpp/preprocess-query (nth input 1) tables)
                                         tables))
                                       (catch js/Error e
                                         (println (str (ex-message e)))))))
          :QueryCommand (reset! fstate/main-result
                                (with-out-str
                                  (try
                                    (pp/print-table
                                     (interp/evaluate
                                      (opti/optimize
                                       (rpp/preprocess-query (nth input 1) tables)
                                       tables)
                                      tables))
                                    (catch js/Error e
                                      (println (str (ex-message e))))))))))))
