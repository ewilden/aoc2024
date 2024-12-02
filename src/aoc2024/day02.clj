(ns aoc2024.day02
  "advent of code 2024 - day 2"
  (:require
   [clojure.string :as str]))

(def day02-input
  (slurp "input/02.txt"))

(def reports
  (->> (str/split-lines day02-input)
       (map #(str/split % #" "))
       (map #(map Integer/parseInt %))))

(defn safe [report]
  (let [steps
        (->> (partition 2 1 report)
             (map (fn [[a b]] (- a b))))
        all_increasing (every? pos? steps)
        all_decreasing (every? neg? steps)
        absolutes (map abs steps)
        big_enough (every? #(>= % 1) absolutes)
        small_enough (every? #(<= % 3) absolutes)]
    ;; (println [steps all_increasing all_decreasing absolutes big_enough small_enough])
    (and (or all_increasing all_decreasing) big_enough small_enough)))

(def part1
  (count (filter safe reports)))

(comment part1)

(defn safe2 [report]
  (let [n (count report)
        remover (fn [n] (fn [col] (keep-indexed (fn [index item] (when (not= index n) item)) col)))
        removers (map remover (range n))
        reports (map (fn [remover report] (remover report)) removers (repeat report))]
    (or (safe report) (some safe reports))))

(safe2 [1 2 10 3])

(def part2 (count (filter safe2 reports)))

(comment part2)
