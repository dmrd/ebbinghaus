(ns ebbinghaus.core
  (:require [reagent.core :as r]
            [re-com.core :as rc]
            [posh.reagent :refer [pull q posh!]]
            [datascript.core :as d]
            [cljsjs.d3 :as d3]
            ))

(defonce db (r/atom {:entities ["SRS", "Flashcards"]}))

(defn update-entities! [f & args]
  (apply swap! db update :entities f args))

(defn add-entity! [e]
  (update-entities! conj e))

(defn new-entry-submit []
  (let [text (r/atom "")]
    (fn []
      [rc/h-box
      :children [[rc/title
                  :label "Enter new item:"]
                  [rc/input-text
                  :model @text
                  :on-change #(reset! text %)]
                  [rc/button
                  :label "Submit"
                   :on-click #(do
                                 (add-entity! @text)
                                 (reset! text ""))

                   ]]])))


(defn show-entries []
   [rc/v-box
    :children (for [e (:entities @db)]
                [:div [:span e]])])

(defn graph-editor []
  [:div
   [new-entry-submit]
   [show-entries]])

(defn render []
  (r/render [graph-editor] (js/document.getElementById "app")))
