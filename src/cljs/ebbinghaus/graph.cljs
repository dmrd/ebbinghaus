(ns ebbinghaus.graph
  (:require
   [reagent.core :as r]
   [re-com.core :as rc]
   [posh.reagent :as p]
   [cljsjs.d3 :as d3]
   [rid3.core :as rid3]
   ))

(def WIDTH 500)
(def HEIGHT 500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viz


(defn get-container []
  (js/d3.select "#graph svg .container .graph")
  )

(defn- avg [l selector]
  (let [src (aget l "source" selector)
        tgt (aget l "target" selector)]
    (/ (+ src tgt) 2)))

;; Actual d3 logic
(defn on-tick [height width link link_text node]
  (fn []
    (.. link
        (attr "x1" #(.. % -source -x))
        (attr "y1" #(.. % -source -y))
        (attr "x2" #(.. % -target -x))
        (attr "y2" #(.. % -target -y)))
    (.. link_text
        (attr "x" #(avg % "x"))
        (attr "y" #(avg % "y")))
    (.. node
        (attr "transform" #(str "translate(" (.. % -x) "," (.. % -y) ")")))
    ))

(defn- build-nodes [svg nodes force-layout]
  (.. svg
      (selectAll ".node")
      (data nodes)
      enter
      (append "text")
      (attr "cx" 12)
      (attr "cy" ".35em")
      (text #(.-name %))
      (call (.-drag force-layout))))

(defn graph-enter [conn]
  (let [
        ; Get all node IDs then their properties
        node_ids @(p/q '[:find ?e :where [?e :name]] conn)
        nodes (for [id (map first node_ids)]
                @(p/pull conn '[:name] id))

        ; Get all relation IDs and properties
        ; TODO(ddohan): Figure out how to get IDs even when they have
        ; duplicate `relation` values
        relation_ids @(p/q '[:find ?e :where [?e :relation]] conn)
        relations (for [id (map first relation_ids)]
                    @(p/pull conn '[:relation :source :target] id))

        ; Convert nodes over to d3 form
        nodes_d3 (clj->js (for [node nodes]
                   {:name (:name node) :id (get-in node [:db/id])}
                   ))
        ; Convert relations over to d3 links
        links_d3 (clj->js (for [relation relations] {
                                         :source (get-in relation [:source :db/id])
                                                     :target (get-in relation [:target :db/id])
                                                     :relation (:relation relation)
                                         }))
        svg (get-container)

        svg_node_group (.. svg
                           (selectAll ".node")
                           (data nodes_d3)
                           (enter)
                           (append "g"))

        svg_link (.. svg
                     (selectAll ".link")
                     (data links_d3)
                     (enter)
                     (append "g")
                     )

        svg_lines (.. svg_link
                      (append "line")
                      (attr "class" "link")
                      (attr "stroke-width" "1.5px")
                      (attr "stroke" "#999")
                      )

        svg_line_text (.. svg_link
                      (append "text")
                      (attr "cx" 12)
                      (attr "cy" ".35em")
                      (text #(.-relation %))
                      )

        ; Setup simulation
        simulation (.. (js/d3.forceSimulation)
                       (force "charge" (.strength (js/d3.forceManyBody) -200))
                       (force "center" (d3.forceCenter (/ WIDTH 2) (/ HEIGHT 2)))
                       ;; (force "x" (js/d3.forceX (/ WIDTH 2)))
                       ;; (force "y" (js/d3.forceY (/ HEIGHT 2)))
                       (force "link" (.. (js/d3.forceLink)
                                         (id (fn [d] d.id))
                                         (distance 50)))
                       (on "tick" (on-tick HEIGHT WIDTH svg_lines svg_line_text svg_node_group)))
        svg_node_rects (.. svg_node_group
                           (append "rect"))
        ]

    (defn- tick []
      (on-tick HEIGHT WIDTH svg_lines svg_line_text svg_node_group)
      )

    (defn- dragstarted [d]
      (simulation.restart)
      (simulation.alphaTarget 1.0)
      (aset d "fx" d.x)
      (aset d "fy" d.y)
      )

    (defn- dragged [d]
      (aset d "fx" d3.event.x)
      (aset d "fy" d3.event.y)
      )

    (defn- dragended [d]
      ; Placeholder
      )

    ; Add the nodes and label SVG objects
    (.. svg_node_rects
                     (attr "class" "node")
                     (attr "y" -20)
                     (attr "height" 40)
                     (attr "rx" 20)
                     (attr "ry" 20)
                     (attr "ry" 20)
                     (attr "stroke-width" "1.5px")
                     (attr "fill" "white")
                     (attr "stroke" "black")
                     (attr "cursor" "move")
                     (call (.. (d3.drag)
                               (on "start" dragstarted)
                               (on "drag" dragged)
                               (on "end" dragended)
                               ))
                     (on "click" (fn [d] (do
                                           (aset d "fx" nil)
                                           (aset d "fy" nil)
                                           ))))

    ; Add node text
    (.. svg_node_group
        (append "text")
        (text (fn [d] d.name))
        (attr "text-anchor" "middle")
        (attr "alignment-baseline" "middle")
        (each (fn [d]
                (this-as this
                 (let [circleWidth 40
                       textLength (.getComputedTextLength this)
                       textWidth (+ textLength 20)
                       ]
                   (aset d "isCircle" true)
                   (aset d "rectX" (/ (- (+ textLength 20)) 2))
                   (aset d "rectWidth" textWidth)
                   (aset d "textLength" textLength)
                   (println d)
                 )))
               )
        )

    ; Change the width of each rectangle to match text width
    (.. svg_node_rects
        (attr "x" (fn [d] d.rectX))
        (attr "width" (fn [d] d.rectWidth))
        )

                                        ; Register the nodes and edges with the simulation
    (simulation.nodes nodes_d3)
    (.. simulation
        (force "link")
        (links links_d3)
        )
    ))

(defn render-graph [conn]
  (let [simulation (.. (js/d3.forceSimulation)
                       (force "charge" (.strength (js/d3.forceManyBody) -200))
                       (force "center" (d3.forceCenter (/ WIDTH 2) (/ HEIGHT 2)))
                       ;; (force "x" (js/d3.forceX (/ WIDTH 2)))
                       ;; (force "y" (js/d3.forceY (/ HEIGHT 2)))
                       (force "link" (.. (js/d3.forceLink)
                                         (id (fn [d] d.id))
                                         (distance 50))))

        prepare-nodes (fn [conn]
                        (let [ids @(p/q '[:find ?e :where [?e :name]] conn)
                              nodes (for [id (map first ids)]
                                      @(p/pull conn '[:name] id))
                              ]
                          (clj->js nodes)
                          ))
        prepare-edges (fn [conn]
                        (let [relation_ids @(p/q '[:find ?e :where [?e :relation]] conn)
                             relations (for [id (map first relation_ids)]
                                         @(p/pull conn '[:relation :source :target] id))]
                          (clj->js relations)
                          ))
        get_x (fn [d]
                (* 100 d.id)
                )
                        ]
    [rid3/viz
     {:id    "graph"
      :ratom conn
      :svg   {:did-mount (fn [node conn]
                           (-> node
                               (.attr "width" 1000)
                               (.attr "height" 1000)
                               (.style "background-color" "grey")))}
      :pieces [
       {:kind :elem-with-data
        :tag "circle"
        :class "nodes"
        :prepare-dataset prepare-nodes
        :did-mount (fn [node ratom]
                     (let [scale js/d3.scale]
                       (println scale)
                       (.. node
                           (attr "cx" get_x)
                           (attr "cy" 100)
                           (attr "r" 30)
                           )
                       ))}

               {:kind :elem-with-data
                :tag "text"
                :class "labels"
                :prepare-dataset prepare-nodes
                :did-mount (fn [node ratom]
                             (.. node
                                 (style "font" "10px sans-serif")
                                 (style "text-anchor" "middle")
                                 (attr "transform" (fn [d] (str "translate(" (get_x d) ", 50)")))
                                 (each (fn [d] (println d)))
                                 (text (fn [d] d.name))
                                 ))}

               {:kind :elem-with-data
                :tag "line"
                :class "edges"
                :prepare-dataset prepare-edges
                :did-mount (fn [node ratom]
                             (.. node
                                 (each (fn [d] (println d)))
                                 (attr "stroke-width" "1.5px")
                                 ))}

               ]}]))

