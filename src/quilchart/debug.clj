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



(ns quilchart.debug
  (:require 
            [quilchart.xOperation :as xOpe]
            [quilchart.color :as color]
            [quilchart.dataGraph :as data]
            )
  (:import [java.time Instant Duration] 
           [org.slf4j Logger LoggerFactory])
  )

;;;;A dot has a x and y position. It is a logical postion, not a displayed postion.
;;(defrecord Dot [logical_x logical_y])
;;;DDot is like Dot but is expected to be used for display coordinate
;;(defrecord DDot [display_x display_y])
;;;Un datafield correspond à l'ensemble des points d'une courbe que l'on veut afficher.
;;;Un datafield possède un nom qui sert d'identifiant au datafield
;;;Il possède en plus une liste de points
;;(defrecord DataField [dfid dots])
;;
;;;;A graph contains a set of datafields curves.
;;;;dataFields is a map of DataField, using a unique id as key (dfid).
;;(defrecord Graph [id dataFields])


(defn dump_graph [logger graph]
    (.debug logger (str "dumping graph  " (:id graph)))
    (doseq [[dfid df] (:dataFields graph)]
      (.debug logger (str "** dfid: " dfid))
      (doseq [dot (:dots df)]
        (.debug logger (str "**** dot: " dot))
      )
    )
  )


(defn dump_full_graph [logger graph]
    (.debug logger (str "dumping graph  " (:id graph)))
    (doseq [[dfid df] (:dataFields graph)]
      (.debug logger (str "** dfid: " dfid))
      (doseq [dot (:dots df)]
        (.debug logger (str "**** dot: " dot))
        (.debug logger (str "**** dotx: " (:display_x dot)))
        (.debug logger (str "**** doty: " (:display_y dot)))

      )
    )
  )
