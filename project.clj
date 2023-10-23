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


(defproject pvittet/quilchart "0.1.0-SNAPSHOT"
  :description "A dynamic time chart system."
  :url "http://example.com/FIXME"
  :license {:name "GPL 3"
            :url "https://www.gnu.org/licenses/gpl-3.0.fr.html#license-text"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [pvittet/quil "3.1.1-SNAPSHOT"]
                 [org.slf4j/slf4j-api "2.0.7"]
                 [ch.qos.logback/logback-classic "1.4.8"]
                 ]
  :plugins [[lein-marginalia "0.9.1"]]
  :aot [quilchart.api]
  )
