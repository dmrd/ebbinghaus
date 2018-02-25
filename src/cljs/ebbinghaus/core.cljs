(ns ebbinghaus.core
  (:require [reagent.core :as r]
            [re-com.core :as rc]
            [posh.reagent :as p]
            [datascript.core :as d]
            [datascript.transit :as dt]
            [cljsjs.d3 :as d3]
            [ebbinghaus.graph :as g]
            ))

(enable-console-print!)


(def schema {:name         {:db/cardinality :db.cardinality/one}
             :relation     {:db/cardinality :db.cardinality/one}
             :source       {:db/valueType :db.type/ref}
             :target       {:db/valueType :db.type/ref}
             :parent       {:db/valueType :db.type/ref}
             :text         {:db/cardinalityType :db.cardinality/one}
             :tag         {:db/cardinalityType :db.cardinality/one}
             })

;; Create a DataScript "connection" (an atom with the current DB value)
(defonce global_conn (d/create-conn schema))
(p/posh! global_conn)

;; Define datoms to init db with
(def datoms [
             ;; {:db/id -1 :name "A"}
             ;; {:db/id -2 :name "B"}
             ;; {:db/id -3 :relation "LINK" :source -1 :target -2}
             {:db/id -1 :name "Datalog" :definition ["A declarative query language"] :properties ["Queries are guaranteed to terminate"]}
             {:db/id -2 :name "pull" :definition ["Retrieve data specified in pull pattern"] :syntax "(pull [conn] [pull pattern] [entity id])"}
             {:db/id -3 :name "q" :definition ["Queries database according to datalog query"] :syntax "(q [query] & args)"}
             {:db/id -4 :name "posh" :definition ["A declarative query language"]}
             {:db/id -5 :relation "wrapper for" :source -4 :target -1}
             {:db/id -6 :relation "function of" :source -2 :target -1}
             {:db/id -7 :relation "function of" :source -3 :target -1}
             {:db/id -8 :tag "root"}
             {:db/id -9 :text "Todos" :parent -8}
             {:db/id -10 :text "Local storage" :parent -9}
             {:db/id -11 :text "Hierarchical display" :parent -9}
             {:db/id -12 :text "Data syncing" :parent -9}
             {:db/id -13 :text "Options" :parent -12}
             {:db/id -14 :text "Firebase" :parent -13}
             {:db/id -15 :text "Datomic" :parent -13}
             ])

(defn db->string [db]
  (dt/write-transit-str db))

(defn string->db [s]
  (dt/read-transit-str s))

;; persisting DB between page reloads
(defn persist_db [conn]
  (js/localStorage.setItem "ebbinghaus/DB" (db->string @conn)))

(defn load_db []
  (let [loaded (js/localStorage.getItem "ebbinghaus/DB")]
    (if (nil? loaded)
      (do
        (.log js/console loaded)
        (println "Creating default database")
        (p/transact! global_conn datoms)
        (persist_db global_conn)
        )
      (do
        (.log js/console "Loaded state from localstorage")
        (let [db (string->db loaded)]
          (reset! global_conn db)
          )))))

(defn transact! [conn query]
  (p/transact! conn query)
  (persist_db conn))

(defn new-entry-submit [conn]
  (let [
        entity_ids @(p/q '[:find ?e :where [?e :name]] conn)
        entities (for [id entity_ids]
                   @(p/pull conn '[:name] (first id)))
        relation_ids @(p/q '[:find ?e :where [?e :relation]] conn)
        relations (for [id relation_ids]
                    @(p/pull conn '[:relation] (first id)))
        new-entity (r/atom "New entity")
        entity (r/atom nil)
        relation (r/atom "relation")
        object (r/atom nil)
        ]
    [:div
     [rc/h-box
      :children [
                 [:span "Enter new item:"]
                 [rc/input-text
                  :model new-entity
                  :change-on-blur? false
                  :on-change #(reset! new-entity %)]
                 [:input {:type "button"
                          :value "Submit"
                          :on-click #(do
                                       (transact! conn [{:db/id -1 :name @new-entity}])
                                       (reset! new-entity ""))}]
                 ]]
     [rc/h-box
      :children [
                 [:span "Enter new relation:"]
                 [rc/single-dropdown
                  :choices entities ;[{:name "test" :id 1}]
                  :id-fn #(:db/id %)
                  :label-fn #(:name %)
                  :model entity
                  :on-change #(reset! entity %)
                  ]
                 [rc/input-text
                  :model relation
                  :change-on-blur? false
                  :width "100px"
                  :on-change #(reset! relation %)]
                 [rc/single-dropdown
                  :choices entities
                  :id-fn #(:db/id %)
                  :label-fn #(:name %)
                  :model object
                  :on-change #(reset! object %)
                  ]
                 [:input {:type "button"
                          :value "Submit relation"
                          :on-click #(transact! conn [{:db/id -1
                                                       :relation @relation
                                                       :source @entity
                                                       :target @object}])
                          }]
                 ]]]))


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

(defn node-edit-ui [conn info]
  (let [text (r/atom (:text info))]
  (fn []
    [:li {:class "div"}
     ;; [:div {:content-editable true
     ;;        :suppressContentEditableWarning true
     ;;        :onChange #(println %)
     ;;        :onBlur #(.log js/console %)
     ;;        }
     [:input {:type "text"
              :value @text
              :style {:border "none"}
              :on-change #(reset! text (-> % .-target .-value))
              :on-blur #(transact! conn [{:db/id (:db/id info)
                                         :text @text
                                         }])
              }
      ]]
    ))
  )

(defn show-tree
  ([conn]
   (let [root_ids @(p/q '[:find ?e :where [?e :tag]] conn)]
     (if (empty? root_ids)
       [:div "No root"]
        (show-tree conn (first (first root_ids)))
     )
     ))
  ([conn id]
   (let [
         ;; children_ids 0
         children_ids @(p/q '[:find ?e
                              :in $ ?parent
                              :where [?e :parent ?parent]] conn id)
         children_info (for [child_vec children_ids]
                                @(p/pull conn '[*] (first child_vec)
                           ))
         ]
     [:div
      (doall (for [info children_info]
               [:ul {:key (:db/id info)}
                [node-edit-ui conn info]
                (show-tree conn (:db/id info))
                ]
               ))
      ]
       ))
  )

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
   [show-tree conn]
   [:hr]
   ; [show-graph conn]
   ])

(defn render []
  (load_db)
  (r/render [graph-editor global_conn] (js/document.getElementById "app")))
