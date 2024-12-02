(ns aoc2024.day01
  "advent of code 2024"
  (:require
   [clojure.string :as str]))

(def day01-input
  (slurp "input/01.txt"))

(def num-lists
  (->> (str/split-lines day01-input)
       (reduce (fn [[accx accy] line]
                 (let [[x y] (str/split line #" +")
                       accx' (conj accx (Integer/parseInt x))
                       accy' (conj accy (Integer/parseInt y))]
                   [accx' accy'])) [])))

(def left (get num-lists 0))
(def right (get num-lists 1))

(comment "day 1 part 1")
(comment
  (->> num-lists
       (map sort)
       (apply (fn [a b] (map vector a b)))
       (map #(- (get % 0) (get % 1)))
       (map abs)
       (reduce +))
  :rcf)

(comment "day 1 part 2")
(comment
  (def right-counts
    (frequencies right))
  (->> left
       (map (fn [elem] (* elem (get right-counts elem 0))))
       (reduce +))
  :rcf)
