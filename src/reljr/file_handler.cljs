(ns reljr.file-handler
  (:require [clojure.walk :refer [stringify-keys]]
            ["papaparse" :as ppr]
            [cljs-bean.core :refer [->clj ->js]]
            [clojure.string :as s]
            [reljr.frontend-state :as fstate]))

(defn download-blob [file-name table]
  (let [csv-str (ppr/unparse (->js (stringify-keys table)))
        blob (js/Blob. #js[csv-str] #js{:type "application/text"})
        object-url (js/URL.createObjectURL blob)
        anchor-element
        (doto (js/document.createElement "a")
          (-> .-href (set! object-url))
          (-> .-download (set! file-name)))]
    (.appendChild (.-body js/document) anchor-element)
    (.click anchor-element)
    (.removeChild (.-body js/document) anchor-element)
    (js/URL.revokeObjectURL object-url)))

(defn make-table
  "take the js table and make it what reljr expects"
  [name js-tab]
  (let [headers (first js-tab)
        rows (rest js-tab)
        headers (map (fn [a] (keyword name a)) headers)]
    {name (into #{} (for [row rows]
                      (into {} (map hash-map headers row))))}))

(defn upload-callback [name jsdata]
  (->> jsdata
       ->clj
       :data
       (make-table (first (s/split name #"\.")))
       (swap! fstate/all-tables merge))
  (swap! fstate/repl-result str "\nread file " name "\n"))

(defn csv-dialog-callback [event]
  (when-let [file (->> event
                       .-target
                       .-files
                       first)]
    (ppr/parse file (->js {:skipEmptyLines true
                           :dynamicTyping true
                           :transform s/trim
                           :complete (partial upload-callback (.-name file))}))))

(defn csv-dialog []
  (let [file-input (.createElement js/document "input")]
    (set! (.-type file-input) "file")
    (set! (.-accept file-input) ".csv")
    (set! (.-onchange file-input) csv-dialog-callback)
    (.click file-input)))
