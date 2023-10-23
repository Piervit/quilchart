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



(ns quilchart.core
  (:require [quilchart.ograph :as ograph] 
            [quilchart.api :as api]
            [quilchart.color :as color]
            [quilchart.dataGraph :as data])
  (:import [java.time Instant]
           [quilchart.ograph Graph]
           [quilchart.ograph Dot]
           [quilchart.ograph DataField])
            )

(def defaultTheme (data/->ThemeData 
                    (color/color 40 40 40 100)  ;background_color
                    (color/color 0 0 255)        ;axis_color
                    (color/color 255 0 0)        ;info_color
                    [ (color/color 0 255 0)        ;datafield1_color
                      (color/color 0 100 100)      ;datafield2_color
                      (color/color 100 100 0)      ;datafield3_color
                      (color/color 50 50 0)        ;datafield4_color
                      (color/color 100 0 100)]      ;datafield5_color
                    )
)

(def default_options 
  {
  :size_win_x 500
  :size_win_y 300
  :title "quilchart"
  :left_border 50
  :right_border 50
  :top_border 50
  :bottom_border 50
  :display_mode :oneScale
  :theme defaultTheme
  }
)

(defn create-instant [milli]
  (Instant/ofEpochMilli milli))

(defn create_dot [i coef]
(data/->Dot (create-instant (* i 10000000 )) 
                              (* (Math/sin (/ i 1000)) coef))
  )


(def dataset1 
  (atom (data/->DataField "First datafield" (map (fn [i] (create_dot i 1))
       (range 1 1000)))))


(def dataset2 
  (atom (data/->DataField "Second datafield" (map (fn [i] (create_dot i 3))
       (range 1 1000)))))



(def graph (data/->Graph (data/get-new-graph-id) {(:dfid @dataset1) @dataset1 (:dfid @dataset2) @dataset2}))


(ograph/create-graph graph default_options)

(while true
  (let [
        curdots (:dots @dataset1)
        ndataset (concat curdots [(create_dot (+ (count curdots) 1) 1)])
        ]
    (Thread/sleep 1000)
    (swap! dataset1 (fn [p] (data/->DataField "First datafield" ndataset)))
    (ograph/update-graph (data/->Graph (:id graph) {(:dfid @dataset1) @dataset1}))
  )
  ) ; 1000 millisecondes = 1 seconde
