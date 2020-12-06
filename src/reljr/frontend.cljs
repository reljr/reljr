(ns reljr.frontend
  (:require [reagent.dom :as rdom]
            [reljr.repl :as repl]
            [reljr.frontend-state :as fstate]
            [reagent.core :as r]))

(def command-history (r/atom []))
(def history-position (r/atom -1))

(defn handle-enter []
  (reset! fstate/current-line (.-value (.getElementById js/document "repl-input")))
  (set! (.-value (.getElementById js/document "repl-input")) "")
  (repl/web-main)
  (swap! fstate/repl-result str "\n" (str "reljr> " @fstate/current-line) "\n" @fstate/main-result)
  (swap! command-history conj @fstate/current-line)
  (swap! history-position inc)
  (reset! fstate/current-line nil)
  (reset! fstate/main-result nil))

(defn get-history-item []
  (let [pos @history-position
        hist @command-history
        num (count hist)]
    (case pos
      0 ""
      (>= pos num) (last hist)
      (nth hist pos))))

(defn app []
  [:div
   [:h1 "reljr"]
   [:h2
    [:a {:href  "https://github.com/LucianoLaratelli/reljr/blob/main/README.org"}
     "Documentation"]]
   [:pre.repl {:id "repl-output"}

    [:p @fstate/repl-result]
    [:p @fstate/file-data]]

   [:input.repl {:style {:width "90%"}
                 :placeholder "reljr> "
                 :id "repl-input"
                 :onKeyDown #(let [key (.-key %)]
                               (case key
                                 "Enter" (handle-enter)
                                 ()))}]])

(defn ^:export main
  []
  (rdom/render [app] (.getElementById js/document "app")))

(defn ^:dev/after-load after-load
  []
  (main))
