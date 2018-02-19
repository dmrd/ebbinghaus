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

(defn node_svg [node]
  (let [color (r/atom "red")]
    (fn []
      [:circle {:r 10
                :cx node.x
                :cy node.y
                :fill @color
                :on-click (fn [] (swap! color rand_color))
                :on-mouse-over (fn [] (println "on"))
                :on-mouse-out (fn [] (println "off"))
                }
       ])
    ))

(defn node_text [node]
  [:text
   {
    :x (+ 10 node.x)
    :y (+ 10 node.y)
    :style {
            :text-anchor "left"
            }
    }
   node.label
   ])

(defn edge_line [g edge]
  (let [
        source (.node g edge.v)
        target (.node g edge.w)
        ]
    [:line {
            :x1 source.x
            :y1 source.y
            :x2 target.x
            :y2 target.y
            :stroke-width "2px"
            :stroke "#000"
            }]))

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

(defn add_nodes [g nodes]
  (doseq [node nodes]
    (.setNode g (:db/id node) (clj->js { :label (:name node) :width 10 :height 10 }))
    )
  g)

(defn add_edges [g edges]
  (doseq [edge edges]
    (.setEdge g (:db/id (:source edge)) (:db/id (:target edge))))
  g
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
        edge_ids @(p/q '[:find ?e :where [?e :relation]] conn)
        edges (for [id (map first edge_ids)]
                @(p/pull conn '[:relation :source :target] id))

        g (-> (.. (js/dagre.graphlib.Graph.)
                  (setGraph {})
                  (setDefaultEdgeLabel (fn [] {}))
                  )
              (add_nodes nodes)
              (add_edges edges)
              )
        ]
    (js/dagre.layout g)
    [:svg {:width WIDTH :height HEIGHT}
     [:g {:transform "translate(250,250)"}
      (for [node (.nodes g)]
        (let [info (.node g node)]
          [:g {:key info.label}
           [node_svg info]
           [node_text info]
           ]))
      (for [edge (.edges g)]
        [:g {:key (str edge.v edge.w)}
         [edge_line g edge]
         [edge_text g edge]
         ])
      ]
     ]))
