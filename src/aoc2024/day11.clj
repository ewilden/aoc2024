(ns aoc2024.day11
  "day 11"
  (:require
    [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def day11-input
  (str/trim (slurp "input/11.txt")))

(def sample
  "125 17")

(def initial-stones (str/split day11-input #" "))
;; (def initial-stones (str/split sample #" "))

(defn normalize [s]
  (let
    [s (str/replace s #"^0+" "")]
    (if (empty? s) "0" s)))

(defn blink [stone]
  (if (= "0" stone) 
    ["1"]
    (if (= 0 (rem (count stone) 2))
      (map normalize 
           [
            (subs stone 0 (quot (count stone) 2))
            (subs stone (quot (count stone) 2))
            ])
      [(str (* 2024 (parse-long stone)))])))

(def part1
  (count (nth (iterate (partial mapcat blink) initial-stones) 25)))

(printf "part1: %d%n" part1)

(def count-after
  (memoize (fn [n stones]
             (if (= 0 n)
               (count stones)
               (->> stones
                    (map #(count-after (- n 1) (blink %)))
                    (reduce +))))))

(printf "part1-memoized: %d%n" (count-after 25 initial-stones))
(printf "part2-memoized: %d%n" (count-after 75 initial-stones))
