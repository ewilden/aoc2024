(ns aoc2024.day10
  "day 10"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def raw-input (slurp "input/10.txt"))
;; (def raw-input
;;   "0123
;; 1234
;; 8765
;; 9876
;; ")

(def day10-input
  (str/trim raw-input))

(def grid
  (->> day10-input
       (str/split-lines)
       (map-indexed
        (fn [r line]
          (map-indexed
           (fn [c item] [[r c] (parse-long item)])
           (str/split line #""))))
       (apply concat)
       (into {})))

(def offsets
  [[1 0]
   [0 1]
   [-1 0]
   [0 -1]])

(defn neighbors [loc]
  (->>
   (map #(map + % loc) offsets)
   (filter #(some? (grid %)))))

(defn edges-from [loc]
  (->> (neighbors loc)
       (filter #(= 1 (- (grid %) (grid loc))))))

(defn step [locs]
  (->>
   (mapcat edges-from locs)
   (into #{})))

(defn score [loc]
  (when (= 0 (grid loc))
    (count
     (nth
      (iterate step #{loc})
      9))))

(def part1
  (->> (keys grid)
       (map score)
       (filter some?)
       (reduce +)))

(defn step-part2 [locs]
  (->>
   (mapcat edges-from locs)
   ))

(defn score-part2 [loc]
  (when (= 0 (grid loc))
    (count
     (nth
      (iterate step-part2 [loc])
      9))))

(def part2
  (->> (keys grid)
       (map score-part2)
       (filter some?)
       (reduce +)))

(printf "part2: %d%n" part2)
