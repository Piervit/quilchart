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


(ns quilchart.api
  (:require [quilchart.ograph :as ograph]
            [quilchart.color :as color] 
            [quilchart.dataGraph :as data])
  
  )



;; generate the full class 
(gen-class
    :name "quilchart.Api"
    :prefix "api-"
    :init "init"
    :constructors {[] []}
    :methods [ 
              [createDisplayGraph [quilchart.dataGraph.Graph] processing.core.PApplet]
              [createEmptyGraph [] quilchart.dataGraph.Graph]
              [updateGraph [quilchart.dataGraph.Graph] void]
              [createEmptyDataField [quilchart.dataGraph.Graph String] quilchart.dataGraph.DataField]
              [addDotsToDataField [quilchart.dataGraph.DataField java.util.List] quilchart.dataGraph.DataField]
              [addDataFieldToGraph [quilchart.dataGraph.Graph quilchart.dataGraph.DataField] quilchart.dataGraph.Graph]
              [createNumXDot [double double] quilchart.dataGraph.Dot]
              [createTimeXDot [java.time.Instant double] quilchart.dataGraph.Dot]
              [liveAddDots [quilchart.dataGraph.Graph String java.util.List] void]
              ]
    )

;Set the framerate of the windows (time between redraw)
(defn api-setFramerate [framerate]
  (ograph/set-framerate framerate)
)

;################# Theme related #################

(def defaultTheme (data/->ThemeData 
                    (color/color 40 40 40 100)  ;background_color
                    (color/color 0 0 255)        ;axis_color
                    (color/color 255 0 0)        ;info_color
                    [ (color/color 0 255 0)        ;datafield1_color
                      (color/color 0 100 100)      ;datafield2_color
                      (color/color 100 100 0)      ;datafield3_color
                      (color/color 50 50 0)        ;datafield4_color
                      (color/color 100 0 100)]      ;datafield5_color
                    (color/color 151 151 151)        ;grid_color
                    )
)

;Set the theme of the windows
(defn api-setTheme [this ntheme]
  (let [api (.api this)] (assoc api :theme ntheme))
  (ograph/set-theme ntheme)
  )

(defn api-getDefaultTheme []
  defaultTheme
  )

(defn api-setBackgroundColor [theme r g b tr]
  (assoc theme :background_color (color/color r g b tr))
)

(defn api-setAxisColor [theme r g b]
  (assoc theme :axis_color (color/color r g b))
)

(defn api-setInfoColor [theme r g b]
  (assoc theme :info_color (color/color r g b))
)

(defn api-setDatafieldColor [theme i r g b]
  (if (< i (count (:datafields_colors theme)))
    (assoc theme :datafields_colors (assoc (:datafields_colors theme) i (color/color r g b) ))
    (assoc theme :datafields_colors (conj ((:datafields_colors theme)) (color/color r g b) ))
))

;################# END Theme related #################

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

(def emptyDataField (data/->DataField "InitialDF" 0 (list )))

;; Create a new empty graph.
(defn api-createEmptyGraph [this]
  (data/->Graph (data/get-new-graph-id) {})
)



(defn api-createEmptyDataField [this graph dfname]
  (assoc emptyDataField :dfid dfname :intid (data/get-new-datafield-id (:id graph)))
)

(defn api-addDataFieldToGraph [this graph dataset]
  (let [curDataSets (:dataFields graph)] 
  (assoc graph :dataFields (assoc curDataSets (:dfid dataset) dataset)))
)

(defn api-addDotsToDataField [this datafield dots]
  (assoc datafield :dots dots))
  ;;(let [curDots (:dots datafield)] 
  ;;(assoc datafield :dots (concat curDots dots)))
  ;;)

(defn api-createDisplayGraph 
  ([this graph] (.get (ograph/create-graph graph default_options)))
  ([this graph options] (.get (ograph/create-graph graph options)))
 )

(defn api-updateGraph 
  ([this graph] (ograph/update-graph graph ) )
 )


(defn api-createNumXDot [this dx dy]
    (data/->Dot dx dy))

(defn api-createTimeXDot [this dx dy]
    (data/->Dot dx dy))


(defn api-liveAddDots [this graph dfid dots]
  (ograph/update-graph-add-dot graph dfid dots)
)

(defn api-init []
  [[] (ref {})])


