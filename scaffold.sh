#!/usr/bin/env bash

,aocdl "$1"
cat > "src/aoc2024/day""$1"".clj" << EOF
(ns aoc2024.day$1
  "day $1"
  (:require
    [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def day$1-input
  (str/trim (slurp "input/$1.txt")))
EOF
