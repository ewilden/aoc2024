(ns aoc2024.day10
  "day 10"
  (:require
    [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def day10-input
  (str/trim (slurp "input/10.txt")))
