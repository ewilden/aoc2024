(ns aoc2024.day03
  "advent of code 2024 - day 3"
  (:require
   [clojure.string :as str]))

(def day03-input
  (slurp "input/03.txt"))

(def matches (re-seq #"mul\((\d+),(\d+)\)" day03-input))

(def part1
  (->> matches
       (map (fn [[_ a b]] (apply * (map Integer/parseInt [a b]))))
       (reduce +)))

(comment part1)

(def part2-matches (re-seq #"mul\(\d+,\d+\)|do\(\)|don't\(\)" day03-input))

(def split-on-do (->> (partition-by (partial re-matches #"do\(\)") part2-matches)
                      (filter #(not= (first %) "do()"))))

(def split-on-dont (->> split-on-do
                        (map (fn [list] (->> list
                                             (take-while (partial not= "don't()")))))))

(def part2
  (->> split-on-dont
       (map (fn [dolist] (->> dolist
                              (map #(re-matches #"mul\((\d+),(\d+)\)" %))
                              (map (fn [[_ a b]] (apply * (map Integer/parseInt [a b]))))
                              (reduce +))))

       (reduce +)))


