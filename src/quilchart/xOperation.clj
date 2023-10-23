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


(ns quilchart.xOperation
  (:require [quilchart.dataGraph :as data])
  (:import [java.time Instant Duration ZoneId]
           [java.time.format DateTimeFormatter]
           [org.slf4j Logger LoggerFactory])
  )


;############# Ce namespace décrit des opérations lié à l'axe des x #############


;init xData_info with graph information
(defn init-xData_info [id_graph xmin xmax ymin ymax xmin_val xmax_val ymin_val ymax_val]
  (if (instance? Instant xmin_val) 
  (let [total_val_duration (Duration/between xmin_val xmax_val)
        ]
    (data/set-xData-info id_graph (data/->XData xmin xmax ymin ymax xmin_val xmax_val ymin_val ymax_val total_val_duration))
    )
    ;else
    (do 
    (data/set-xData-info id_graph (data/->XData xmin xmax ymin ymax xmin_val xmax_val ymin_val ymax_val (- xmax_val xmin_val)))
    )
  ))


;Return the min between a and b.
(defn xmin[a b]
  (if (instance? Instant a) 
    (; comparaison sur des Instant
       if (.isBefore a b) a b)
    (; comparaison sur des entiers
       min a b)
    ))

;Return true if a < b
(defn lower [a b]
  (if (instance? Instant a) 
    (; comparaison sur des Instant
       .isBefore a b)
    (; comparaison sur des entiers
       < a b)
    )
)

;Return the max between a and b.
(defn xmax[a b]
  (if (instance? Instant a) 
    (; comparaison sur des Instant
       if (.isAfter a b) a b)
    (; comparaison sur des entiers
       max a b)
    ))

;Return true if a > b
(defn higher [a b]
  (if (instance? Instant a) 
    (; comparaison sur des Instant
       .isAfter a b)
    (; comparaison sur des entiers
       > a b)
    )
)
;ratio correspond au ratio de position de x entre les bornes min_x et max_x
;marche aussi pour l'axe y
;Il est compris entre 0 et 1.
(defn ratio [min_x max_x x]
  (if (instance? Instant x) 
    ;Si l'on travaille sur des Instants
    (let [max_duration (Duration/between min_x max_x)
          x_duration (Duration/between min_x x)
          ]
      (/ (.getSeconds x_duration) (.getSeconds max_duration))
      )
    ;Sinon
    (let [max_duration (- max_x min_x )
          x_duration (- x min_x )
          ]
      (/ x_duration max_duration) 
      )
    ))


;; Managing quick-ratio stuff using reference.
;; The goal is to create function similar to the ratio [min_x max_x x].

(def _max_duration_x (atom nil))
(def _max_duration_y (atom nil))

(defn set-x-min-max-quick-ratio [min_x max_x]
  (if (instance? Instant min_x) 
    (swap! _max_duration_x (fn [_] {:min_x min_x :max_x max_x :max_duration (.getSeconds (Duration/between min_x max_x))}))
    (swap! _max_duration_x (fn [_] {:min_x min_x :max_x max_x :max_duration (- max_x min_x)}))
    )
  )

;ratio correspond au ratio de position de x entre les bornes min_x et max_x
;marche aussi pour l'axe y
;Il est compris entre 0 et 1.
(defn quick-ratio-x [x]
  (if (instance? Instant x) 
    ;Si l'on travaille sur des Instants
    (let [x_duration (Duration/between (:min_x @_max_duration_x) x)
          ]
      (/ (.getSeconds x_duration) (:max_duration @_max_duration_x))
      )
    ;Sinon
    (let [
          x_duration (- x (:min_x @_max_duration_x))
          ]
      (/ x_duration  (:max_duration @_max_duration_x)) 
      )
    ))


(defn set-y-min-max-quick-ratio [min_y max_y]
    (swap! _max_duration_y (fn [_] {:min_y min_y :max_y max_y :max_duration (- max_y min_y)}))
  )

;ratio correspond au ratio de position de x entre les bornes min_y et max_y
;marche aussi pour l'axe y
;Il est compris entre 0 et 1.
(defn quick-ratio-y [x]
    (let [
          y_duration (- x (:min_y @_max_duration_y))
          ]
      (/ y_duration  (:max_duration @_max_duration_y)) 
      )
    )



(def possible_time_unit #{:int :second :minut :hour :day :week :month :year :centuries})

;retourne un time unit adapté a l'interval fourni
(defn _evaluate_time_unit [min_x max_x]
  (if (instance? Instant min_x)
    ;c'est un type Instant 
    (let [x_duration (Duration/between min_x max_x)
          x_duration_sec (.getSeconds x_duration)]
      (cond 
        (< x_duration_sec 240) :second ;less than 240 sec means unit is second
        (< x_duration_sec 10800) :minut  ;less than 3 hour sec means unit is minut
        (< x_duration_sec 259200) :hour  ;less than 3 day means unit is hour
        (< x_duration_sec 2635200) :day  ;less than 3 mouth means unit is day
        (< x_duration_sec 94608000) :month  ;less than 3 year means unit is mouth
        (< x_duration_sec 9460800000) :year  ;less than 300 years means unit is mouth
        :else :centuries)
      )
    :int
    )
  ;Sinon c'est un numérique
  )

;Return the x value associated the the given x-position
(defn xPosToVal [id_graph xpos]
  (let [xData_info (data/get-xData-info id_graph)
        ratio (ratio (:min_x xData_info) (:max_x xData_info) xpos)
        ]
    (if (instance? Instant (:min_x_val xData_info) )
    (; c'est un type Instant
     .plus (:min_x_val xData_info)  (.dividedBy (.multipliedBy (:total_val_duration xData_info) (* ratio 10000)) 10000))
    (; sinon, double
     + (:min_x_val xData_info)  (* (:total_val_duration xData_info) ratio ) )
    )
  ))

;Return the y value associated the the given y-position
(defn yPosToVal [id_graph ypos]
  (let [xData_info (data/get-xData-info id_graph)
        ratio (ratio (:max_y xData_info) (:min_y xData_info)  ypos)
        ]
    (+ (:min_y_val xData_info) (* (- (:max_y_val xData_info) (:min_y_val xData_info)) ratio))
    )
  )

;Return a string representation of x.
(defn xStr [id_graph x]
  (let [xData_info (data/get-xData-info id_graph)
        time_unit (_evaluate_time_unit (:min_x_val xData_info) (:max_x_val xData_info))
        dateTimeFormatter 
        (cond 
          (= time_unit :second)  (DateTimeFormatter/ofPattern "HH:mm:ss:SS")
          (= time_unit :minut)  (DateTimeFormatter/ofPattern "HH:mm:ss:SS")
          (= time_unit :hour)  (DateTimeFormatter/ofPattern "dd HH:mm")
          (= time_unit :day)  (DateTimeFormatter/ofPattern "MM-dd HH:mm")
          (= time_unit :week)  (DateTimeFormatter/ofPattern "MM-dd HH:mm")
          (= time_unit :month)  (DateTimeFormatter/ofPattern "yyyy-MM-dd")
          (= time_unit :year)  (DateTimeFormatter/ofPattern "yyyy-MM-dd")
          (= time_unit :centuries)  (DateTimeFormatter/ofPattern "yyyy-MM-dd")
          )
        ]

    (cond 
      (= time_unit :int) (str x) ;formatter undefined for :int 
      :else 
      (let [
          dateTimeFormatter (.withZone dateTimeFormatter  (ZoneId/systemDefault))]
      (.format dateTimeFormatter x))
      ))
  )

(defn yStr [id_graph y]
  (str (yPosToVal id_graph y))
  )

