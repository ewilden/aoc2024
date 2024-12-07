(ns aoc2024.day07
  "day 7"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])
(require '[clojure.math :as math])

(def day07-input
  (slurp "input/07.txt"))

(def equations
  (->> day07-input
       (str/split-lines)
       (map #(str/split % #": "))
       (map (fn [[lhs rhs]] [(parse-long lhs) (mapv parse-long (str/split rhs #" "))]))))

(count equations)

(defn try-mul [target nums]
  (when (= (rem target (peek nums)) 0) [(quot target (peek nums)) (pop nums)]))

(defn try-add [target nums]
  (when (>= target (peek nums)) [(- target (peek nums)) (pop nums)]))

(defn try-equation [target nums]
  (if (= 1 (count nums))
    (= (first nums) target)
    (or
     (match (try-mul target nums)
       nil nil
       [target' nums'] (try-equation target' nums'))
     (match (try-add target nums)
       nil nil
       [target' nums'] (recur target' nums')))))

(def part1 (->> equations
                (filter #(apply try-equation %))
                (map first)
                (reduce +)))

(defn try-concat [target nums]
  (let [targetstr (str target)
        n (peek nums)
        nstr (str n)]
    (when (str/ends-with? targetstr nstr) [(long  (quot target (math/pow 10 (count nstr))))
                                           (pop nums)])))

(defn try-equation-part2 [target nums]
  (if (= 1 (count nums))
    (= (first nums) target)
    (or
     (match (try-mul target nums)
       nil nil
       [target' nums'] (try-equation-part2 target' nums'))
     (match (try-concat target nums)
       nil nil
       [target' nums'] (try-equation-part2 target' nums'))
     (match (try-add target nums)
       nil nil
       [target' nums'] (recur target' nums')))))

(def part2 (->> equations
                (filter #(apply try-equation-part2 %))
                (map first)
                (reduce +)))

(print part2)
