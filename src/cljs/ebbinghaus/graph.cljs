(ns ebbinghaus.graph
  (:require
   [reagent.core :as r]
   [re-com.core :as rc]
   [posh.reagent :as p]
   [cljsjs.d3 :as d3]))

(def WIDTH 500)
(def HEIGHT 500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viz


(defn get-container []
  (js/d3.select "#graph svg .container .graph")
  )

(defn container-enter [conn]
  (-> (js/d3.select "#graph svg")
      (.append "g")
      (.attr "class" "container")
      (.attr "width" WIDTH)
      (.attr "height" HEIGHT)
      ))

(defn container-did-mount [conn]
  (container-enter conn))

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
        ]

    (defn- tick []
      (on-tick HEIGHT WIDTH svg_lines svg_line_text svg_node_group)
      )

    ;; (println simulation.fix)

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
    (.. svg_node_group
                     (append "circle")
                     (attr "class" "node")
                     (attr "r" 6)
                     (call (.. (d3.drag)
                               (on "start" dragstarted)
                               (on "drag" dragged)
                               (on "end" dragended)
                               ))
                     (on "click" (fn [d] (do
                                           (aset d "fx" nil)
                                           (aset d "fy" nil)
                                           ))))

    (.. svg_node_group
        (append "text")
        (text (fn [d] d.name ))
        (attr "transform" "translate(5,-4)"))

    ; Register the nodes and edges with the simulation
    (simulation.nodes nodes_d3)
    (.. simulation
        (force "link")
        (links links_d3)
        )
    ))

;; Reagent wrappers


(defn graph-update [conn])
(defn graph-exit [conn]
  [])

(defn graph-did-update [conn]
  (graph-enter conn)
  (graph-update conn)
  (graph-exit conn))


(defn graph-did-mount [conn]
  (.. (js/d3.select "#graph svg .container")
      (append "g")
      (attr "class" "graph")
      (attr "transform" "translate(0,20)")
      )
  (graph-did-update conn))



;; Main

(defn viz-render [conn]
  (let [width  WIDTH
        height HEIGHT]
    [:div
     {:id "graph"}
     [:svg
      {:width  width
       :height height}]]))

(defn viz-did-mount [conn]
  (container-did-mount conn)
  (graph-did-mount conn))

(defn viz-did-update [conn]
  (graph-did-update conn))

(defn- render-graph [conn]
  (r/create-class
   {:reagent-render      #(viz-render conn)
    :component-did-mount #(viz-did-mount conn)
    :component-did-update #(viz-did-update conn)}))
