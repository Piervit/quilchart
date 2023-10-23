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



(ns quilchart.drawOneScale
  (:require [quil.core :as q]
            [quilchart.color :as color]
            [quilchart.xOperation :as xOpe]
            [quilchart.dataGraph :as data]
            )
  (:import [org.slf4j Logger LoggerFactory])
  )
;;This is the base drawing module. It uses a single scale for every datafields.

(def logger (LoggerFactory/getLogger "quilchart"))

;;global function allowing every inner function to access theme prog_options and internal_info as internal vars
(defn draw-state [theme prog_options internal_info mouse_status has_zoom graph-has-changed graph]

  (defn save-display-graph [dgraph]
    (data/set-prev-display-graph (:id graph) dgraph)
  )


  (defn set-stroke [col]
    (q/stroke (color/red col) (color/green col) (color/blue col)))

  (defn set-fill-color [col]
    (q/fill (color/red col) (color/green col) (color/blue col)))


  (defn write_text_on_x [text x] 
    (set-stroke (:info_color theme))
    (set-fill-color (:info_color theme))
    (let [  
          left_border (:left_border prog_options)
          dot_xy_meet_y (+ (:height_y_axis internal_info) (:top_border prog_options))
          ]
      (q/line (+ x left_border) (- dot_xy_meet_y 5) (+ x left_border) (+ dot_xy_meet_y 5))
      (q/text text (- (+ x left_border) 10) (+ dot_xy_meet_y 20))
      (set-stroke (:axis_color theme))
      ))


  (defn write_text_on_y [text y] 
    (q/text-align :right )
    (q/line (- (:left_border prog_options) 5) y (+ (:left_border prog_options) 5) y)
    (q/text text (- (:left_border prog_options) 3) y )
    (q/text-align :left )
    )

  ; Dessine l'axe des X prend en compte les valeurs des points pour afficher des valeurs
  (defn draw-x-axis [graph]
    (set-stroke (:axis_color theme))
    (let [
          left_border (:left_border prog_options)
          width_x_axis (:width_x_axis internal_info)
          dot_xy_meet_y (+ (:height_y_axis internal_info) (:top_border prog_options))
          x_axis (:x_axis graph)
          timeunit (:timeunit x_axis)]
      ;trace l'axe
      (q/line left_border dot_xy_meet_y (+ width_x_axis left_border) dot_xy_meet_y)
      ;Place un texte à l'emplacement 0
      (write_text_on_x (xOpe/xStr (:id graph) (:xmin_val internal_info)) 0)
      ;Place un texte à l'emplacement terminal
      (write_text_on_x (xOpe/xStr (:id graph) (:xmax_val internal_info)) width_x_axis)
      ;(q/text (xOpe/xStr (:id graph) (:max_x @_internal_info)) (- width_x_axis 10) (+ dot_xy_meet_y 15))

      )
    )

  ; Dessine l'axe des Y prend en compte les valeurs des points pour afficher des valeurs
  (defn draw-y-axis [graph]
    (set-stroke (:axis_color theme))
    (let [
          height_y_axis (:height_y_axis internal_info)
          ]
      (q/line (:left_border prog_options) 
              (+ height_y_axis (:top_border prog_options)) 
              (:left_border prog_options) 
              (:top_border prog_options))
      ;write text on max y 
      (set-stroke (:info_color theme))
      (set-fill-color (:info_color theme))
      (write_text_on_y (str (:ymax_val internal_info)) (:top_border prog_options))
      ;write text on min y 
      (write_text_on_y (str (:ymin_val internal_info)) (+ (:height_y_axis internal_info) (:top_border prog_options)))
      (set-stroke (:axis_color theme))
      )
    )

  (defn convert_x_to_draw [logical_x]
    (let 
      ;ratio correspond au ratio de position de logical_x entre les bornes min_x et max_x
      [
       width_x_axis (:width_x_axis internal_info)
       ratio (xOpe/quick-ratio-x logical_x)]
      (+ (* width_x_axis ratio) (:left_border prog_options))
      )
    )

  (defn convert_y_to_draw [logical_y]
    (let 
      ;ratio correspond au ratio de position de logical_x entre les bornes min_x et max_x
      [
       height_y_axis (:height_y_axis internal_info)
       ;ok xOpe also work for y
       ratio (xOpe/quick-ratio-y logical_y)]
      (+ (-  height_y_axis (* height_y_axis ratio) ) (:top_border prog_options))
      )
    )

  (defn put-data-quick [datafield]
    (def prev_dot (atom nil))
    (set-stroke (nth (:datafields_colors theme) (:intid datafield)))
    (doall (map (fn [dot] 
                  (if (nil? @prev_dot) 
                    (q/point (:display_x dot) (:display_y dot)) 
                    (q/line (:display_x @prev_dot) (:display_y @prev_dot) (:display_x dot) (:display_y dot)))
                  (swap! prev_dot (fn [p] (data/->DDot (:display_x dot) (:display_y dot)))))
                (:dots datafield))))


  ;Print the dots in the graph.
  (defn put-data [datafield]
    (xOpe/set-x-min-max-quick-ratio (:xmin_val internal_info) (:xmax_val internal_info))
    (xOpe/set-y-min-max-quick-ratio (:ymin_val internal_info) (:ymax_val internal_info))
    (defn is-inside-zoom [dot]
      (or has_zoom
          (and (xOpe/lower (:logical_x dot) (:xmax_val internal_info)) 
               (and (xOpe/higher (:logical_x dot) (:xmin_val internal_info))
                    (and (xOpe/lower (:logical_y dot) (:ymax_val internal_info))
                         (xOpe/higher (:logical_y dot) (:ymin_val internal_info))))))
      )


    (set-stroke (nth (:datafields_colors theme) (:intid datafield)))
    (def prev_dot (atom nil))
    (assoc datafield :dots (reduce
                             (fn [lst dot] 
                               (let [
                                     final_x (convert_x_to_draw (:logical_x dot))
                                     final_y (convert_y_to_draw (:logical_y dot))
                                     ]
                                 (if (is-inside-zoom dot)
                                   (if (nil? @prev_dot) 
                                     (q/point final_x final_y) 
                                     (q/line (:display_x @prev_dot) (:display_y @prev_dot) final_x final_y ))
                                   ())
                                 (let [pdot (data/->DDot final_x final_y) ]
                                   (swap! prev_dot (fn [p] pdot))
                                   (conj lst pdot)
                                   )
                                 )
                               )
                             (list)
                             (:dots datafield)
                             )

           )
    )

  ;Paint the background using the active theme
  (defn paint-background []
    (let [back_red (color/red (:background_color theme))
          back_green (color/green (:background_color theme))
          back_blue (color/blue (:background_color theme))
          back_alpha (color/alpha (:background_color theme))
          ]
      (q/background back_red back_green back_blue back_alpha)
      ))


  (defn draw-mouse-info [graph]
    (set-stroke (:info_color theme))
    (let [mouse_x (q/mouse-x)
          mouse_y (q/mouse-y)
          left_lim (:left_border prog_options)
          right_lim (+ (:left_border prog_options) (:width_x_axis internal_info))
          top_lim (:top_border prog_options)
          bot_lim (+ (:top_border prog_options) (:height_y_axis internal_info))
          is-inside-graph (fn [x y graph] (and (and (and (> x left_lim) (< x right_lim)) (> y top_lim)) (< y bot_lim)))
          ]
      (data/set-draw-mouse-pos (:id graph) {:coordx mouse_x :coordy mouse_y})
      ;si la souris est dans l'espace du graph, dessine les alignements par rapports à celle-ci.
      (if (is-inside-graph mouse_x mouse_y graph)
        (do
          ;dessine une ligne entre l'axe des y et la position de la souris
          (q/line left_lim mouse_y mouse_x mouse_y)
          ;dessine une ligne entre l'axe des x et la position de la souris
          (q/line mouse_x mouse_y mouse_x bot_lim)
          (write_text_on_x (xOpe/xStr (:id graph) (xOpe/xPosToVal (:id graph) (- mouse_x left_lim))) (- mouse_x left_lim ))
          (write_text_on_y (xOpe/yStr (:id graph) (- mouse_y (:top_border prog_options))) mouse_y )
          )
        nil
        )
      ))

  (defn is-currently-zooming [graph]
    (not (nil? (:mouse-presed-start mouse_status)))
    )

  ;Draw the zoom beiing currently zoomed by the user (if any).
  (defn draw-zoom-zone [graph]
    (if (not (nil? (:mouse-presed-start mouse_status)))
      (let[
           x (:coordx (:mouse-presed-start mouse_status))
           y (:coordy (:mouse-presed-start mouse_status))
           width (- (q/mouse-x) x)
           height (- (q/mouse-y) y)
           ]
        (q/fill 200 200 200 40)
        (q/rect x y width height)
        )
      ()
      )
    )

  (defn draw-grid [graph]
    (let [
          left_border (:left_border prog_options)
          top_border (:top_border prog_options)
          width_x_axis (:width_x_axis internal_info)
          nb_line 5
          space_between_line (/ (:height_y_axis internal_info) nb_line)
          dot_xy_meet_y (+ (:height_y_axis internal_info) (:top_border prog_options))
          horz_first_stroke (- dot_xy_meet_y space_between_line)
          horz_sec_stroke (- dot_xy_meet_y (* space_between_line 2))
          horz_thrd_stroke (- dot_xy_meet_y (* space_between_line 3))
          horz_fourth_stroke (- dot_xy_meet_y (* space_between_line 4))
          horz_five_stroke (- dot_xy_meet_y (* space_between_line 5))
          ]
      (set-stroke (:grid_color theme))
      (set-fill-color (:grid_color theme))
      (q/line left_border horz_first_stroke (+ width_x_axis left_border) horz_first_stroke)
      (write_text_on_y (xOpe/yStr (:id graph) (- horz_first_stroke top_border)) horz_first_stroke)
      (q/line left_border horz_sec_stroke (+ width_x_axis left_border) horz_sec_stroke)
      (write_text_on_y (xOpe/yStr (:id graph) (- horz_sec_stroke top_border)) horz_sec_stroke)
      (q/line left_border horz_thrd_stroke (+ width_x_axis left_border) horz_thrd_stroke)
      (write_text_on_y (xOpe/yStr (:id graph) (- horz_thrd_stroke top_border)) horz_thrd_stroke)
      (q/line left_border horz_fourth_stroke (+ width_x_axis left_border) horz_fourth_stroke)
      (write_text_on_y (xOpe/yStr (:id graph) (- horz_fourth_stroke top_border)) horz_fourth_stroke)
      (q/line left_border horz_five_stroke (+ width_x_axis left_border) horz_five_stroke)
    )
    )

(defn init-draw-state [graph]
  (paint-background )
  ; dessin de l'axe des Y
  (if (not (data/get-need-pause (:id graph)))
    (do
     (draw-y-axis graph)
     ; dessin de l'axe des X
     (draw-x-axis graph)
     ; dessin du quadrillage
     (draw-grid graph)
    (if (and (or (not graph-has-changed) (is-currently-zooming graph)) (not (nil? (data/get-prev-display-graph (:id graph) ))))
      (update-vals (data/get-prev-display-graph (:id graph)) put-data-quick)
      (save-display-graph (update-vals (:dataFields graph) put-data))
     )
     ;(doall (map-indexed put-data (vals (:dataFields graph))))
     (draw-mouse-info graph)
     (draw-zoom-zone graph)
     )
    ()
    )
  )

  (init-draw-state graph)
)

