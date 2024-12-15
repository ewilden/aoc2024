(ns aoc2024.day14
  "day 14"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def day14-input
  (str/trim (slurp "input/14.txt")))

(def sample
  "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(def use-sample
  false
  ;; true
  )

(def input
  (if use-sample sample day14-input))

(def width (if use-sample 11 101))
(def height (if use-sample 7 103))

(defn parse-line [line]
  (let [[_ px py vx vy] (re-matches #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)" line)
        [px py vx vy] (map parse-long [px py vx vy])]
    {:position [px py] :velocity [vx vy]}))

(def robots
  (->> (str/split-lines input)
       (map parse-line)))

(def quadrant-predicates
  (for [xcmp [< >]
        ycmp [< >]]
    (fn [[x y]] (and (xcmp x (quot width 2)) (ycmp y (quot height 2))))))

(defn pick-quadrant [loc]
  (let [idx (.indexOf (map #(% loc) quadrant-predicates) true)]
    (when (>= idx 0) idx)))

(defn move-robot [seconds robot]
  (let [{[px py] :position
         [vx vy] :velocity} robot
        px' (+ px (* seconds vx))
        py' (+ py (* seconds vy))]
    [(mod px' width)
     (mod py' height)]))

(def final-positions
  (->> robots
       (map (partial move-robot 100))
       (sort)))

(def part1
  (->> final-positions
       (map pick-quadrant)
       (filter some?)
       (group-by identity)
       (vals)
       (map count)
       (reduce *)))

(printf "part1: %d%n" part1)

(defn print-positions [positions]
  (let [positions (into #{} positions)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (print (if (positions [x y]) \@ \.)))
      (print "\n"))))

(def same-row-threshold 15)

(defn consecutive-1s [ls]
  (reductions (fn [acc n]
                (if (= n 1)
                  (+ acc 1)
                  0)) 0 ls))

(defn has-enough-consecutive [seconds]
  (let [positions (->> robots
                       (map (partial move-robot seconds)))
        by-y (group-by (fn [[_ y]] y) positions)]
    (->> (vals by-y)
         (map (fn [points]
                (let [xs (into #{} (map first points))
                      xs (sort xs)
                      steps (map (fn [[x1 x2]] (- x2 x1)) (partition 2 1 xs))]
                  (apply max (consecutive-1s steps)))))
         (some #(>= % (- same-row-threshold 1))))))

(def christmas-tree-seconds
  (->> (range)
       (filter has-enough-consecutive)
       (first)))

(printf "christmas-tree-seconds: %d%n" christmas-tree-seconds)

(print-positions (map (partial move-robot christmas-tree-seconds) robots))
