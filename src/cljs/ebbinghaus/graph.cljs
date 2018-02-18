(ns ebbinghaus.graph
  (:require
   [reagent.core :as r]
   [re-com.core :as rc]
   [posh.reagent :as p]
   [rid3.core :as rid3]
   ))

(def WIDTH 500)
(def HEIGHT 500)

(defn- avg [l selector]
  (let [src (aget l "source" selector)
        tgt (aget l "target" selector)]
    (/ (+ src tgt) 2)))

(defn render-graph [conn]
  (let [
        force (.. js/cola
                  (d3adaptor js/d3)
                  (linkDistance 120)
                  )

        prepare-graph (fn [conn]
                        (let [node_ids @(p/q '[:find ?e :where [?e :name]] conn)
                              nodes (for [id (map first node_ids)]
                                      @(p/pull conn '[:name] id))
                              relation_ids @(p/q '[:find ?e :where [?e :relation]] conn)
                              relations (for [id (map first relation_ids)]
                                          @(p/pull conn '[:relation :source :target] id))

                                        ; Get node id -> idx in the node array
                                        ; and change `links` to be relative to it.
                              id_map (into {}
                                           (for [idx (range (count nodes))]
                                             [(:db/id (nth nodes idx)) idx]))
                              relations (for [relation relations]
                                          {
                                           :relation (get relation :relation)
                                           :source (get id_map (get-in relation [:source :db/id]))
                                           :target (get id_map (get-in relation [:target :db/id]))
                                           }
                                          )
                              nodes_js (clj->js nodes)
                              edges_js (clj->js relations)
                              ]
                          (.. force
                              (nodes nodes_js)
                              (links edges_js)
                              (start 100)
                              )
                          (clj->js
                           (concat
                            (js->clj nodes_js)
                            (js->clj edges_js)))
                          ))
        ]
    [rid3/viz
     {:id    "graph"
      :ratom conn
      :svg   {:did-mount (fn [node conn]
                           (-> node
                               (.attr "width" WIDTH)
                               (.attr "height" HEIGHT)
                               (.style "background-color" "grey")))}
      :pieces [
               {:kind :elem-with-data
                :tag "g"
                :class "ids"
                :prepare-dataset prepare-graph
                :did-mount (fn [node ratom]
                             (let [
                                   ; Filter down to nodes
                                   nodes (.. node
                                             (filter (fn [d] (do (println d) d.name)))
                                             (attr "transform" (fn [d] (str "translate(" (+ 250 d.x) "," (+ 250 d.y) ")")))
                                             )
                                   ; Filter down to edges
                                   edges (.. node
                                             (filter (fn [d] d.source))
                                             (attr "transform" (fn [d] (str "translate(250,250)")))
                                             )
                                   ]

                               (.. nodes
                                   (append "circle")
                                   (attr "r" 10)
                                   )

                               (.. nodes
                                   (append "text")
                                   (attr "transform" "translate(10,10)")
                                   (style "font" "10px sans-serif")
                                   (style "text-anchor" "left")
                                   (each (fn [d] (println d)))
                                   (text (fn [d] d.name))
                                   )

                               (.. edges
                                   (append "line")
                                   (attr "stroke-width" "2px")
                                   (attr "stroke" "#fff")
                                   (attr "x1" (fn [d] d.source.x))
                                   (attr "y1" (fn [d] d.source.y))
                                   (attr "x2" (fn [d] d.target.x))
                                   (attr "y2" (fn [d] d.target.y))
                                   )

                               (.. edges
                                   (append "text")
                                   (text (fn [d] d.relation))
                                   (style "font" "10px sans-serif")
                                   (style "text-anchor" "left")
                                   (attr "x" (fn [d] (avg d "x")))
                                   (attr "y" (fn [d] (avg d "y")))
                                   )
                               )

                             )}


               ]}]))

