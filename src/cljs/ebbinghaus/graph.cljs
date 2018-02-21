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
  (let [src (get-in l ["source" selector])
        tgt (get-in l ["target" selector])]
    (/ (+ src tgt) 2)))
(defn- rand_color []
  (rand-nth ["green" "black" "grey" "red" "blue" "purple" "yellow"]))

(defn svg_render [node _]
  (-> node
      (.attr "width" WIDTH)
      (.attr "height" HEIGHT)
      (.style "background-color" "grey")))

(defn node_svg [pos]
  (let [color (r/atom "red")]
    (fn []
      [:circle {:r 10
                :cx (:x pos)
                :cy (:y pos)
                :fill @color
                :on-click (fn [] (swap! color rand_color))
                :on-mouse-over (fn [] (println "on"))
                :on-mouse-out (fn [] (println "off"))
                }
       ])
    ))

(defn node_to_position [node]
  (let [pos (.-position (.-_private node))]
    {
     :x (* 100 pos.x)
     :y (* 100 pos.y)
     }
    ))

(defn edge_to_source [edge]
  (.-source (.-_private edge))
  )

(defn edge_to_target [edge]
  (.-target (.-_private edge))
  )

(defn edge_to_relation [edge]
  (.-relation (.-_private edge))
  )

(defn node_text [node pos]
  [:text
   {
    :x (+ 10 (:x pos))
    :y (+ 10 (:y pos))
    :style {
            :text-anchor "left"
            }
    }
   (aget node "_private" "data" "label")
   ])

(defn edge_line [source target]
  [:line {
          :x1 (:x source)
          :y1 (:y source)
          :x2 (:x target)
          :y2 (:y target)
          :stroke-width "2px"
          :stroke "#000"
          }])

(defn edge_text [edge source target]
  [:text
   {
    :x (/ (+ (:x source) (:x target)) 2)
    :y (/ (+ (:y source) (:y target)) 2)
    :style {
            :text-anchor "left"
            }
    }
   edge._private.data.relation
   ]
  )

(defn add_nodes [g nodes]
  (doseq [node nodes]
    (.add g (clj->js {:group "nodes"
                      :data
                      { :id (:db/id node) :label (:name node) :width 10 :height 10 }}))
    )
  g)

(defn add_edges [g edges]
  (doseq [edge edges]
    (.add g (clj->js {:group "edges"
                      :data
                      { :id (:db/id edge)
                       :source (:db/id (:source edge))
                       :target (:db/id (:target edge))
                       :width 10 :height 10 }})))
  g)

(defn build_graph [ratom]
  (let [conn (:conn @ratom)
        node_ids @(p/q '[:find ?e :where [?e :name]] conn)
        nodes (for [id (map first node_ids)]
                @(p/pull conn '[:name] id))
        edge_ids @(p/q '[:find ?e :where [?e :relation]] conn)
        edges (for [id (map first edge_ids)]
                @(p/pull conn '[:relation :source :target] id))

        g (js/cytoscape.
           (clj->js {
                     :layout {
                              :name "circle"
                              }
                     :elements {
                                :nodes (clj->js (for [node nodes]
                                                  (clj->js {:group "nodes"
                                                            :data
                                                            { :id (:db/id node) :label (:name node) :width 10 :height 10 }})))
                                :edges (clj->js (for [edge edges]
                                                  {:group "edges"
                                                   :data
                                                   {
                                                    :id (:db/id edge)
                                                    :relation (:relation edge)
                                                    :source (:db/id (:source edge))
                                                    :target (:db/id (:target edge))
                                                    }}))
                                }
                     })
           )
        ]
    g))

(defn render-graph [ratom]
  (let [g (build_graph ratom)
        nodes_js (.nodes g "")
        edges_js (.edges g "")
        ]
    [:svg {:width WIDTH :height HEIGHT}
     [:g {:transform "translate(250,250)"}
      (for [node_id (range nodes_js.length)]
        (let [node (aget nodes_js node_id)
              pos (node_to_position node)
              ]
          [:g {:key node_id}
           [node_svg pos]
           [node_text node pos]
           ]
          ))
      (for [edge_id (range edges_js.length)]
        (let [edge (aget edges_js edge_id)
              source (edge_to_source edge)
              source_pos (node_to_position source)

              target (edge_to_target edge)
              target_pos (node_to_position target)
              ]
          [:g {:key edge_id}
           [edge_line source_pos target_pos]
           [edge_text edge source_pos target_pos]
           ]
          ))
      ]
     ]))
