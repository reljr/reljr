(ns reljr.frontend-state
  (:require
   [reagent.core :as r :refer [atom]]))

;;input text from user
(def current-line (r/atom ""))

;;contains all output showed to the user in the repl
(def repl-result (r/atom [:div "welcome to reljr\n\n"
                          [:a {:href
                               "https://github.com/reljr/reljr/blob/main/README.org"}  "read the reljr documentation\n"]
                          [:a {:href
                               "https://github.com/reljr/reljr"}  "read the reljr source\n"]
                          [:a {:href
                               "https://github.com/reljr/reljr/blob/main/resources/foo.csv"}  "download an example table (and read it with `read`)\n\n"]]))


;; all of the tables that have been read-in or stored


(def all-tables (r/atom {}))

;;file data for upload cmd
(def file-data (atom nil))

;;results of web-main calls get put here
(def main-result (r/atom nil))

