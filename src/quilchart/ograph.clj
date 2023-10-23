;;
;;Copyright (C) 2023 Pierre Vittet <pvittet|at|murena|dot|io>
;;
;;This program is free software: you can redistribute it and/or modify
;;it under the terms of the GNU General Public License as published by
;;the Free Software Foundation, either version 3 of the License, or
;;(at your option) any later version.
;;
;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU General Public License for more details.
;;
;;You should have received a copy of the GNU General Public License
;;along with this program.  If not, see <https://www.gnu.org/licenses/>.



(ns quilchart.ograph
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quilchart.xOperation :as xOpe]
            [quilchart.color :as color]
            [quilchart.drawOneScale :as drawOneScale]
            [quilchart.dataGraph :as data]
            ;[quilchart.utils :as utils])
            )
  (:import [java.time Instant Duration] 
           [java.lang System]
           [org.slf4j Logger LoggerFactory])
  )


;############### END Types expected to be visible externally. ###############
(def logger (LoggerFactory/getLogger "quilchart"))

;;
;;This function compute and return the internal info of a datafield
(defn fill-internal-datafield [datafield]
  (defn minmax_xy_point_of_datafield [df]
    (reduce (fn [[acc_min_x acc_min_y acc_max_x acc_max_y] dot] 
              (let [
                    new_min_x (if (nil? acc_min_x) (:logical_x dot) (xOpe/xmin (:logical_x dot) acc_min_x))
                    new_min_y (if (nil? acc_min_y) (:logical_y dot) (min (:logical_y dot) acc_min_y)) 
                    new_max_x (if (nil? acc_max_x) (:logical_x dot) (xOpe/xmax (:logical_x dot) acc_max_x)) 
                    new_max_y (if (nil? acc_max_y) (:logical_y dot) (max (:logical_y dot) acc_max_y)) 
                    ]
                  [new_min_x new_min_y new_max_x new_max_y]
              )) [nil nil nil nil] (:dots df))
    )
  (let [[min_x min_y max_x max_y] (minmax_xy_point_of_datafield datafield)
        ]
    (data/->IntDataField
      (:dfid datafield)
      min_x
      max_x
      min_y
      max_y
      )
    )
)

;this function fill the internal_info ref without considering zoom stuff.
(defn fill-internal-info-with-zoom [size_win_x size_win_y graph]

  (let [
        prog_options (data/get-prog-options (:id graph))
        zoom_management (data/get-zoom-management (:id graph))
        left_border (:left_border prog_options)
        right_border (:right_border prog_options)
        bottom_border (:bottom_border prog_options)
        top_border (:top_border prog_options)
        int_datafields (update-vals (:dataFields graph) fill-internal-datafield)
        internal_graph (data/->IntGraph 
                         size_win_x                                      ;;size_win_x
                         size_win_y                                      ;;size_win_y
                         (- (- size_win_x left_border) right_border)     ;;width_x_axis
                         (- (- size_win_y bottom_border) top_border)     ;;height_y_axis
                         int_datafields
                         (:zoom_min_x zoom_management)
                         (:zoom_max_x zoom_management)  
                         (:zoom_min_y zoom_management)
                         (:zoom_max_y zoom_management)
                         )
        ]

    (data/set-graph-internal (:id graph) internal_graph)
    (xOpe/init-xData_info (:id graph)
                          0 
                          (:width_x_axis internal_graph) 
                          0 
                          (:height_y_axis internal_graph) 
                          (:zoom_min_x zoom_management)
                          (:zoom_max_x zoom_management)
                          (:zoom_min_y zoom_management)
                          (:zoom_max_y zoom_management)
                          )
    (data/set-graph-has-changed (:id graph) true)
    (data/set-need-redraw (:id graph) true)
    )
  )


;this function fill the internal_info ref without worriying about zoom stuff.
(defn fill-internal-info-no-zoom [size_win_x size_win_y graph]
  (let [
        prog_options (data/get-prog-options (:id graph))
        left_border (:left_border prog_options)
        right_border (:right_border prog_options)
        bottom_border (:bottom_border prog_options)
        top_border (:top_border prog_options)
        int_datafields (update-vals (:dataFields graph) fill-internal-datafield )
                                   
        min_x (reduce-kv (fn [acc k datafield] 
                        (let [min_df (:xmin_val datafield)] 
                          (if (nil? acc) min_df (xOpe/xmin acc min_df))))
                      nil int_datafields)
        max_x (reduce-kv (fn [acc k datafield] 
                        (let [max_df (:xmax_val datafield)] 
                          (if (nil? acc ) max_df (xOpe/xmax acc max_df))))
                      nil int_datafields)
        min_y (reduce-kv (fn [acc k datafield] 
                        (let [min_df (:ymin_val datafield)] 
                          (if (nil? acc ) min_df (min acc min_df))))
                      nil int_datafields)
        max_y (reduce-kv (fn [acc k datafield] 
                        (let [max_df (:ymax_val datafield)] 
                          (if (nil? acc ) max_df (max acc max_df))))
                      nil int_datafields)
        internal_graph (data/->IntGraph 
                         size_win_x                                      ;;size_win_x
                         size_win_y                                      ;;size_win_y
                         (- (- size_win_x left_border) right_border)     ;;width_x_axis
                         (- (- size_win_y bottom_border) top_border)     ;;height_y_axis
                         int_datafields
                         min_x
                         max_x
                         min_y
                         max_y
                         )
        ]
    (data/set-graph-internal (:id graph) internal_graph)
    (xOpe/init-xData_info (:id graph) 0 (:width_x_axis internal_graph) 0
                          (:height_y_axis internal_graph) min_x max_x min_y
                          max_y)
    (data/set-graph-has-changed (:id graph) true)
    (data/set-need-redraw (:id graph) true)
    )
  )

;This function fills the internal info of the graph (for rapid displaying)
(defn prepare-info-and-data [size_win_x size_win_y graph]
  (if (nil? (data/get-zoom-management (:id graph)))
    (fill-internal-info-no-zoom  size_win_x size_win_y graph)
    (fill-internal-info-with-zoom  size_win_x size_win_y graph)
    )
  ;;(transform-to-display-dots graph)
)


(defn add-dots-to-current-graph [graph df_dots_map]
    (doseq [[dfid dots] df_dots_map]
      (let [
            dfs (:dataFields graph)
            df (get dfs dfid)
            df_dots (:dots df)
            new_df (assoc df :dots (concat df_dots dots))
            new_dfs (assoc dfs dfid new_df)
            ]
        (data/set-current-graph (:id graph) (assoc graph :dataFields new_dfs))
        (data/set-graph-has-changed (:id graph) true)
        ))
)

;; This function modify the internal info of a graph by taking into account the dots added.
(defn add-dots-to-internal-info [graph df_dots_map]
  (doseq [[dfid dots] df_dots_map]
    (let [
          internal_info (data/get-graph-internal (:id graph))
          zoom_management (data/get-zoom-management (:id graph))
          ;true if at least one element is in the zoom (or if there is no zoom)
          is-out-of-zoom (fn [dot] 
                           (and (not (nil? zoom_management)) ; if zoom
                                (or (xOpe/higher (:logical_x dot) (:xmax_val internal_info)) 
                                (or (xOpe/lower (:logical_x dot) (:xmin_val internal_info))
                                (or (xOpe/higher (:logical_y dot) (:ymax_val internal_info))
                                    (xOpe/lower (:logical_y dot) (:ymin_val internal_info))))))
                           )
          ]

      (if (some is-out-of-zoom dots)
        (;if zoom and the dot not in it, no change, acc_df
             ;do nothing
             )
        ;else
        (let [
              int_dfs (:internal_datafields internal_info)
              int_df (get int_dfs dfid)
              new_int_df (reduce 
                           (fn [acc_df dot] 
                             (let [
                                   new_min_x (if (nil? (:xmin_val acc_df)) 
                                               (:logical_x dot) 
                                               (xOpe/xmin (:logical_x dot) (:xmin_val acc_df)))
                                   new_min_y (if (nil? (:ymin_val acc_df)) 
                                               (:logical_y dot) 
                                               (min (:logical_y dot) (:ymin_val acc_df))) 
                                   new_max_x (if (nil? (:xmax_val acc_df)) 
                                               (:logical_x dot) 
                                               (xOpe/xmax (:logical_x dot) (:xmax_val acc_df))) 
                                   new_max_y (if (nil? (:ymax_val acc_df)) 
                                               (:logical_y dot) 
                                               (max (:logical_y dot) (:ymax_val acc_df))) 
                                   ]
                               (data/->IntDataField
                                 (:dfid acc_df)
                                 new_min_x
                                 new_max_x
                                 new_min_y
                                 new_max_y
                                 )
                               )) int_df dots)
              ;;min_x at the internal_info level (for every df)
              min_x (if (nil? (:xmin_val internal_info)) (:xmin_val new_int_df) (xOpe/xmin (:xmin_val new_int_df) (:xmin_val internal_info)))
              max_x (if (nil? (:xmax_val internal_info)) (:xmax_val new_int_df) (xOpe/xmax (:xmax_val new_int_df) (:xmax_val internal_info)))
              min_y (if (nil? (:ymin_val internal_info)) (:ymin_val new_int_df) (min (:ymin_val new_int_df) (:ymin_val internal_info)))
              max_y (if (nil? (:ymax_val internal_info)) (:ymax_val new_int_df) (max (:ymax_val new_int_df) (:ymax_val internal_info)))
              ]
          (data/set-graph-internal (:id graph) (assoc internal_info
                                                      :internal_datafields (assoc
                                                                             int_dfs
                                                                             dfid
                                                                             new_int_df)
                                                      :xmin_val min_x 
                                                      :xmax_val max_x 
                                                      :ymin_val min_y
                                                      :ymax_val max_y))
          (xOpe/init-xData_info (:id graph) 0 (:width_x_axis internal_info) 0
                                (:height_y_axis internal_info) min_x max_x min_y
                                max_y)
          (data/set-need-redraw (:id graph) true)
          )
        )
      )
    ))

;Update the framerate
(defn set-framerate [fr]
  (if (integer? fr)
    ;si entier
    (do 
      (q/frame-rate fr)
      )
    ;else
    (.error logger (str "Error: calling set-framework with a non-integer argument: " fr))
    ))


;this function simply set mouse-presed-start and mouse-presed-end
(defn detect-zoom [graph]
  (if (nil? (data/get-mouse-presed-start (:id graph)))
    (if (and (q/mouse-pressed?) (= :left (q/mouse-button)))
      ;This in an init of mouse presed
      (data/set-mouse-presed-start (:id graph) {:coordx (q/mouse-x) :coordy (q/mouse-y)})
      ()
      )
    ; case where mouse-presed-start is not null, we then monitor the release
    (if (and (q/mouse-pressed?) (= :left (q/mouse-button)))
      ()
      (data/set-mouse-presed-end (:id graph) {:coordx (q/mouse-x) :coordy (q/mouse-y)})
      )
    )
  )

(defn do-zoom-graph [graph]
  (let [
        id_graph (:id graph)
        mouse-presed-start (data/get-mouse-presed-start (:id graph))
        mouse-presed-end (data/get-mouse-presed-end (:id graph))
        start_x (:coordx mouse-presed-start)
        start_y (:coordy mouse-presed-start)
        end_x (:coordx mouse-presed-end)
        end_y (:coordy mouse-presed-end)
        prog_options (data/get-prog-options (:id graph))
        min_x (- (if (< start_x end_x) start_x end_x) (:left_border prog_options))
        min_y (- (if (> start_y end_y) start_y end_y) (:top_border prog_options))
        max_x (- (if (< start_x end_x) end_x start_x) (:left_border prog_options))
        max_y (- (if (> start_y end_y) end_y start_y) (:top_border prog_options))
        internal_info (data/get-graph-internal (:id graph))
        ]
  (data/set-zoom-management id_graph 
                            (data/->ZoomManagement 
                            true 
                            (xOpe/xPosToVal id_graph min_x) 
                            (xOpe/xPosToVal id_graph max_x) 
                            (xOpe/yPosToVal id_graph min_y) 
                            (xOpe/yPosToVal id_graph max_y)))
  (xOpe/init-xData_info id_graph 0 (:width_x_axis internal_info) 0
                        (:height_y_axis internal_info) min_x max_x min_y
                        max_y)
  (data/set-need-recompute (:id graph) true)
  )
  )

(defn handle-zoom [graph]
  (detect-zoom graph)
  (if (and (not (nil? (data/get-mouse-presed-start (:id graph)))) (not (nil? (data/get-mouse-presed-end (:id graph)))))
    (do;zoom action has been done by the usser
      (do-zoom-graph graph)
      (data/set-mouse-presed-start (:id graph) nil)
      (data/set-mouse-presed-end (:id graph) nil)
     )
    ()
    )
  ;; If we click the right button, we remove zoom
  (if (and (q/mouse-pressed?) (= :right (q/mouse-button)))
    (do (data/set-zoom-management (:id graph) nil)
        ;we should also recompute the internal info
        (data/set-need-recompute (:id graph) true)
    )
    ()
    )
)

; Return true if the mouse has moved, else return false.
(defn has-moved-mouse  [graph] 
  (let [cur_x (q/mouse-x)
        cur_y (q/mouse-y)
        prev_mouse_pos (data/get-draw-mouse-pos (:id graph))
        has_moved (if (not (nil? prev_mouse_pos))
                    (not (and (= (:coordx prev_mouse_pos) cur_x) (= (:coordy prev_mouse_pos) cur_y)))
                    true
                    )
        ]
        has_moved
   )
)

; Just check if the mouse has moved to generate redrawing of the graph
(defn handle-mouse-pos [graph] 
  (if (has-moved-mouse graph) 
    (do
      (data/set-need-redraw (:id graph) true))
    ()
  )
)

;; Check both the zoom detection and the mouse mouvement
(defn mouse-management [graph]
  (handle-mouse-pos graph)
  (handle-zoom graph)
  )

;; This function is called at each iteration of a skech if need-recompute flag is true.
;; It is used to update the state of the graph.
;; It has to compute every internal info of the graph. It has to prepare everything for the drawing to be fast.
;; I also should recompute only what is needed.
(defn prepare-draw [graph_id] 
  (let [
        prog_options (data/get-prog-options graph_id)
        width_x_axis (- (- (q/width) (:left_border prog_options)) (:right_border prog_options))
        height_y_axis (- (- (q/height) (:bottom_border prog_options)) (:top_border prog_options))
        internal_info (data/get-graph-internal graph_id)
        ]
    (data/set-graph-internal graph_id (assoc internal_info :size_win_y (q/height) :height_y_axis  height_y_axis :size_win_x (q/width) :width_x_axis width_x_axis))
    )
  (let [new_graph (data/get-new-graph graph_id)]
    ;There is third scenario.
    (if (not( nil? new_graph))
      ;Scenario 1 : If @_new_graph is not null it means, there is a new graph and we recompute everything.
      (do
        (data/set-current-graph graph_id new_graph)
        (prepare-info-and-data (q/width) (q/height) new_graph)
        ;;As the graph has been updated, new-graph go back to nil.
        (data/set-new-graph graph_id nil)
        )
      ;;else
      (let [current_graph (data/get-current-graph graph_id)]
        ;;Scenario 2 : There is no need to recompute every data but some dots must be added.
        (if (data/get-dots-to-add graph_id)
          (do ;;we update the current graph.
              (add-dots-to-current-graph current_graph (data/get-dots-to-add graph_id))
              ;;then we update the linked internal info
              (add-dots-to-internal-info current_graph (data/get-dots-to-add graph_id)) 
              (data/set-dots-to-add graph_id nil)
              )
          ;;else
          ;Scenario 3: There is no new graph change, but we need to recompute the internal info. (This usually comes from the zoom management.)
          (prepare-info-and-data (q/width) (q/height) current_graph)
        )
        ))
    (data/set-need-recompute graph_id false)
    (data/get-current-graph graph_id)
  ))

;;This mainly checks if windows size has change, implying redrawing.
(defn windows-management [graph]
  (let [ 
        internal_info (data/get-graph-internal (:id graph))
        xdiff (- (q/width) (:size_win_x internal_info))
        ydiff (- (q/height) (:size_win_y internal_info))
        wd-redraw-at (data/get-wd-resize-at (:id graph))
        ]
    ;;(.debug logger (str "xdiff: " wdiff))
    (if (or (not (= 0 xdiff)) (not (= 0 ydiff)))
      (do
        (data/set-need-pause (:id graph) true)
        (data/set-wd-resize-at (:id graph) (System/currentTimeMillis))
        (data/set-graph-internal (:id graph) (assoc internal_info :size_win_x (q/width) :size_win_y (q/height)))
        )
      (if (not (nil? wd-redraw-at))
        (let [curTime (System/currentTimeMillis)
              timeDiff (- curTime wd-redraw-at)
              ]
          (if (> timeDiff 300) 
            (do 
              (data/set-wd-resize-at (:id graph) nil)
              (data/set-need-pause (:id graph) false)
              (data/set-need-recompute (:id graph) true)
              (data/set-need-redraw (:id graph) true)
              )
            ()
            ))
        ()
        )
      )
    )
  )

;This function is called at the start of the sketch only.
;It is only used to set the frame rate. (But the framerate could also be modified later).
(defn setup [graph]
  (fn [] 
    (set-framerate 30)
    (prepare-draw (:id graph))
    )
  )



(defn update-state [graph] 
  (mouse-management graph)
  (windows-management graph)
  (if (data/get-need-recompute (:id graph))
    ;(time 
      (prepare-draw (:id graph))
      ;)
    graph
    )
)

(defn create-instant [milli]
  (Instant/ofEpochMilli milli))

(defn draw-state [graph]
  (if (data/get-need-redraw (:id graph)) ;Only redraw if needed
    (let [mouse_status {:mouse-presed-start (data/get-mouse-presed-start (:id graph)) :mouse-presed-end (data/get-mouse-presed-end (:id graph))}
          graph_theme (data/get-graph-theme (:id graph))
          internal_info (data/get-graph-internal (:id graph))
          prog_options (data/get-prog-options (:id graph))
          zoom_management (data/get-zoom-management (:id graph))
          ]

      (case (:display_mode prog_options)
        :oneScale (do 
                    ;(time 
                    (drawOneScale/draw-state graph_theme prog_options internal_info mouse_status (nil? zoom_management) (data/get-graph-has-changed (:id graph)) graph)
                    (data/set-graph-has-changed (:id graph) false)
                    )
                    ;)
        )
      (data/set-need-redraw (:id graph) false)
      )
    ())
  )


(defn set-theme [graph theme]
  (data/set-graph-theme (:id graph) theme)
  )


(defn create-graph [graph options]
  (data/set-prog-options (:id graph) options)
  (data/set-zoom-management (:id graph) nil)
  (let [size_win_x (:size_win_x options)
        size_win_y (:size_win_y options)
        title (:title options)
        ]
    (set-theme graph (:theme options))
    (prepare-info-and-data size_win_x size_win_y graph)
    (data/set-new-graph (:id graph) graph)
    (data/set-need-recompute (:id graph) true)

    (let [ sketch (q/defsketch quilchart
      :size [size_win_x size_win_y]
      ; setup function called only once, during sketch initialization.
      :setup (setup graph)
      ; update-state is called on each iteration before draw-state.
      :update update-state
      :draw draw-state
      :features [:keep-on-top :resizable ]
      :renderer :fx2d
      ; This sketch uses functional-mode middleware.
      ; Check quil wiki for more info about middlewares and particularly
      ; fun-mode.
      :middleware [m/fun-mode]
      )]
    sketch
    )
    )
  )

;;Full update of a graph: we recompute every data from the graph.
(defn update-graph [graph]
  (data/set-new-graph (:id graph) graph)
  (data/set-need-recompute (:id graph) true)
)

;;Minor update of a graph: we add a single dot the an existing dataField of a graph.
;;dots is a vector of dot to add.
(defn update-graph-add-dot [graph dfid dots]
  (data/set-dots-to-add (:id graph) {dfid dots})
  (data/set-need-recompute (:id graph) true)
  )
