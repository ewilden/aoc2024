(ns aoc2024.day13
  "day 13"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def day13-input
  (str/trim (slurp "input/13.txt")))

(def sample
  "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(def input
  day13-input
  ;; sample
  )

(defn parse-button [line]
  (let [[_ x y] (re-matches #"Button .: X\+(\d+), Y\+(\d+)" line)]
    (mapv parse-long [x y])))

;; (parse-button "Button A: X+94, Y+34")

(defn parse-prize [line]
  (let [[_ x y] (re-matches #"Prize: X=(\d+), Y=(\d+)" line)]
    (mapv parse-long [x y])))

;; (parse-prize "Prize: X=8400, Y=5400")

(def machines
  (->> (str/split input #"\n\n")
       (map (fn [section]
              (let [[a b prize] (str/split-lines section)]
                {:a (parse-button a)
                 :b (parse-button b)
                 :prize (parse-prize prize)})))))
(comment
  xa * a + xb * b = xp
  ya * a + yb * b = yp
  a = (xp - (xb * b)) / xa
  (ya/xa) * (xp - (xb * b)) + yb * b = yp
  (ya * xp / xa) - (ya * xb / xa) * b + yb * b = yp
  (yb - (ya * xb / xa)) * b = yp - (ya * xp / xa)
  b = (yp - (ya * xp / xa)) / (yb - (ya * xb / xa)))

(defn solve-system [machine]
  (let [[xa ya] (machine :a)
        [xb yb] (machine :b)
        [xp yp] (machine :prize)
        b (/ (- yp (/ (* ya xp) xa))
             (- yb (/ (* ya xb) xa)))
        a (/ (- xp (* xb b)) xa)]
    [a b]))

(def part1
  (->> machines
       (map solve-system)
       (filter (fn [presses] (every? #(not (ratio? %)) presses)))
       (map (fn [[a b]] [(* 3 a) b]))
       (reduce (partial reduce +) 0)))

(printf "part1: %d%n" (biginteger part1))

(def machines-part2
  (map #(update % :prize (partial map (partial + 10000000000000))) machines))

(def part2
  (->> machines-part2
       (map solve-system)
       (filter (fn [presses] (every? #(not (ratio? %)) presses)))
       (map (fn [[a b]] [(* 3 a) b]))
       (reduce (partial reduce +) 0)))

(printf "part2: %d%n" (biginteger part2))
