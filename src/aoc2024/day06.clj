(ns aoc2024.day06
  "day 6"
  (:require
   [clojure.string :as str])
  (:require
   [clojure.core.reducers :as r]))

(def day06-input
  (slurp "input/06.txt"))
;; (def day06-input
;; (slurp "input/06-sample.txt"))

(def grid
  (str/split-lines day06-input))

(defn lookup [r c] (get (get grid r) c))

(defn offset [facing]
  (case facing
    :up [-1 0]
    :down [1 0]
    :right [0 1]
    :left [0 -1]))

(defn turn [facing]
  (case facing
    :up :right
    :right :down
    :down :left
    :left :up))

(defn unchecked-step [loc facing]
  (map + loc (offset facing)))

(unchecked-step [0 0] :up)

(defn step [& {loc :loc facing :facing}]
  (let [stepped (unchecked-step loc facing)]
    (case (apply lookup stepped)
      nil nil
      \. {:loc stepped :facing facing}
      \^ {:loc stepped :facing facing}
      \# {:loc loc :facing (turn facing)})))

(defn visited-from [so-far state]
  (loop [so-far so-far
         state state]
    (let [so-far (conj so-far (:loc state))
          stepped (step state)]
      (if stepped
        (recur so-far stepped)
        so-far))))

(def start (->> grid
                (keep-indexed (fn [r row] (let [c (str/index-of row \^)]
                                            (when (not= c nil) [r c]))))
                (first)))

(def visited-for-part1 (visited-from #{} {:loc start :facing :up}))

(def part1 (count visited-for-part1))

(defn step-part2 [barrier & {loc :loc facing :facing}]
  (let [stepped (unchecked-step loc facing)
        stepped'
        (case (apply lookup stepped)
          nil nil
          \. {:loc stepped :facing facing}
          \^ {:loc stepped :facing facing}
          \# {:loc loc :facing (turn facing)})]
    (when stepped' (if (= (:loc stepped') barrier) {:loc loc :facing (turn facing)} stepped'))))

(step-part2 [0 0] {:loc [0 1] :facing :left})

(lookup 0 0)

(defn causes-loop [barrier so-far state]
  (loop [so-far so-far
         state state]
    (if (contains? so-far state)
      true
      (let [so-far (conj so-far state)
            stepped (step-part2 barrier state)]
        (if stepped
          (recur so-far stepped)
          false)))))

(def part2 (->> grid
                (map-indexed (fn [r row] (map-indexed (fn [c _] [r c]) (range (count row)))))
                (apply concat)
                (filter (partial not= start))
                (filter (partial contains? visited-for-part1))
                (r/filter #(causes-loop % #{} {:loc start :facing :up}))
                (r/map (fn [_] 1))
                (r/fold (r/monoid + (constantly 0)))))

(print part2)

