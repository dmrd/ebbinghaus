(ns ebbinghaus.core
  (:require [reagent.core :as r]
            [re-com.core :as rc]
            [posh.reagent :as p]
            [datascript.core :as d]
            [cljsjs.d3 :as d3]
            [ebbinghaus.graph :as g]
            ))

(enable-console-print!)

(def schema {:name         {:db/cardinality :db.cardinality/one}
             :relation     {:db/cardinality :db.cardinality/one}
             :source       {:db/valueType :db.type/ref}
             :target       {:db/valueType :db.type/ref}
             })

;; Create a DataScript "connection" (an atom with the current DB value)
(def conn (d/create-conn schema))

;; Define datoms to init db with
(def datoms [
             {:db/id -1 :name "Datalog" :definition ["A declarative query language"] :properties ["Queries are guaranteed to terminate"]}
             {:db/id -2 :name "pull" :definition ["Retrieve data specified in pull pattern"] :syntax "(pull [conn] [pull pattern] [entity id])"}
             {:db/id -3 :name "q" :definition ["Queries database according to datalog query"] :syntax "(q [query] & args)"}
             {:db/id -4 :name "posh" :definition ["A declarative query language"]}
             {:db/id -5 :relation "wrapper for" :source -4 :target -1}
             {:db/id -6 :relation "function of" :source -2 :target -1}
             {:db/id -7 :relation "function of" :source -3 :target -1}
             ])

;;; Add the datoms via transaction
(d/transact! conn datoms)

;; Register the connection with posh
(p/posh! conn)

; datalog is declarative, logic programming language
; datalog queries are guaranteed to terminate

(defn new-entry-submit [conn]
  (let [text (r/atom "")]
    (fn []
      [:div
       [:div [:span "Enter new item:"]]
       [:div [rc/input-text
              :model @text
              :on-change #(reset! text %)]
        [:input {:type "button"
                 :value "Submit"
                 :on-click #(do
                              (p/transact! conn [{:db/id -1 :name @text}])
                              (reset! text ""))
                 }]]])))

(def get-names '[:find ?n
                 :where
                 [?e :name ?n]
                 ])

(defn show-entity [conn id]
  (let [e @(p/pull conn '[:name] id)]
    [:div
     [:span (str id ". " (:name e))]
     ]
    ))

(defn show-entries [conn]
  (let [ids @(p/q '[:find ?e :where [?e :name]] conn)]
    [rc/v-box
     :children (for [id (map first ids)] (show-entity conn id))]))

(defn show-graph [conn]
  [:div
   [:span "graph"]
   [g/render-graph conn]
   ]
  )


(defn graph-editor [conn]
  [:div
   [new-entry-submit conn]
   [:hr]
   [show-entries conn]
   [:hr]
   [show-graph conn]
   ])

(defn render []
  (r/render [graph-editor conn] (js/document.getElementById "app")))
