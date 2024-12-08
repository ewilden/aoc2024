(ns aoc2024.day08
  "day 8"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def day08-input
  (slurp "input/08.txt"))

(def grid
  (->> day08-input
       (str/split-lines)
       (mapv #(mapv identity (str/split % #"")))))


