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

(defn svg_render [node _]
  (-> node
      (.attr "width" WIDTH)
      (.attr "height" HEIGHT)
      (.style "background-color" "grey")))

(defn node_svg [node]
  (let [color (r/atom "red")]
    (fn []
      [:circle {:r 10
                :cx (get node "x")
                :cy (get node "y")
                :fill @color
                ;; :on-click (fn [] (swap! color ))
                :on-mouse-over (fn [] (println "on"))
                :on-mouse-out (fn [] (println "off"))
                }
       ])
    ))

(defn node_text [node]
  [:text
   {
    :x (+ 10 (get node "x"))
    :y (+ 10 (get node "y"))
    :style {
            :text-anchor "left"
            }
    }
   (get node "name")
   ])

(defn edge_line [edge]
  [:line {
          :x1 (get-in edge ["source" "x"])
          :y1 (get-in edge ["source" "y"])
          :x2 (get-in edge ["target" "x"])
          :y2 (get-in edge ["target" "y"])
          :stroke-width "2px"
          :stroke "#000"
          }])

(defn edge_text [edge]
  [:text
   {
    :x (avg edge "x")
    :y (avg edge "y")
    :style {
            :text-anchor "left"
            }
    }
   (get edge "relation")
   ]
  )

(defn render-graph [ratom]
  (let [state (r/atom {
                       :edges []
                       :nodes []
                       })
        conn (:conn @ratom)
        node_ids @(p/q '[:find ?e :where [?e :name]] conn)
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

        ; TODO(ddohan): Figure out a way to avoid the switching between mutable and immutable
        nodes_js (clj->js nodes)
        edges_js (clj->js relations)
        force (.. js/cola
                  (d3adaptor js/d3)
                  (linkDistance 120)
                  (nodes nodes_js)
                  (links edges_js)
                  (start 100)
                  )
        nodes_clj (js->clj nodes_js)
        edges_clj (js->clj edges_js)
        prepare-graph (fn [ratom] (concat (js->clj edges_js) (js->clj nodes_js)))
        ]
    [:svg {:width WIDTH :height HEIGHT}
     [:g {:transform "translate(250,250)"}
      (for [node nodes_clj]
        [:g {:key (get node "index")}
         [node_svg node]
         [node_text node]
         ])
      (for [idx (range (count edges_clj))]
        (let [edge (get edges_clj idx)]
          [:g {:key idx}
           [edge_line edge]
           [edge_text edge]
           ]))
      ]
     ]))
