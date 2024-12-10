(ns aoc2024.day09
  "day 9"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

;; (def day09-input
;;   (str/trim (slurp "input/09-sample.txt")))
(def day09-input
  (str/trim (slurp "input/09.txt")))

(def files
  (->> (seq day09-input)
       (map (fn [chr] (Character/digit chr 10)))
       (map vector (interpose :space (range)))
       (mapcat (fn [[index length]] (repeat length index)))))

(def compacted-length (count (filter (partial not= :space) files)))

(def reduce-to-fill (reduce
                     (fn [[output reved] item]
                       (let [reved (drop-while (partial = :space) reved)]
                         (match [item (first reved)]
                           [:space n] [(conj output n) (rest reved)]
                           [n _] [(conj output n) reved])))
                     [[] (reverse files)]
                     files))

(def part1
  (->> (first reduce-to-fill)
       (take compacted-length)
       (map-indexed (fn [blockpos fileno] (* blockpos fileno)))
       (reduce +)))

(printf "part1: %d%n" part1)



