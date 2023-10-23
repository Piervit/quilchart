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



(ns quilchart.color
  (:require [quil.core :as q])
)


(defrecord Color [red green blue alpha])

(defn color
  ([red green blue] (->Color red green blue 0))
  ([red green blue alpha] (->Color red green blue alpha))
  )

(defn red [color]
  (assert (instance? Color color))
  (:red color))

(defn green [color]
  (assert (instance? Color color))
  (:green color))

(defn blue [color]
  (assert (instance? Color color))
  (:blue color))

(defn alpha [color]
  (assert (instance? Color color))
  (:alpha color))
  

(defn colorToQColor [color]
  (q/color (:red color) (:green color) (:blue color) (:alpha color))
  )
