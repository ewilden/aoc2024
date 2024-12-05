(ns aoc2024.day04
  "day 4"
  (:require
   [clojure.string :as str]))

(def day04-input
  (slurp "input/04.txt"))

(def lines (str/split-lines day04-input))

(def twodarr (mapv #(str/split % #"") lines))

;; (defn transpose [a] (apply mapv vector a))
;; (def transposed-twodarr (transpose twodarr))

(def patterns-abstract
  [[:asc :fixed]
   [:asc :asc]
   [:asc :desc]
   [:fixed :asc]
   [:fixed :desc]
   [:desc :fixed]
   [:desc :asc]
   [:desc :desc]])

(defn to-concrete [choice]
  (case choice
    :asc [0 1 2 3]
    :fixed [0 0 0 0]
    :desc [0 -1 -2 -3]))

(def patterns-concrete
  (mapv (fn [[a b]] [(to-concrete a) (to-concrete b)]) patterns-abstract))

(defn get-with-pattern [[rpat cpat] [r c]]
  (->> (map vector rpat cpat)
       (map (fn [[roff coff]] [(+ roff r) (+ coff c)]))
       (map #(get-in twodarr %))))

(comment
  (= ["S" "M" "M" "M"]
     (get-with-pattern (get patterns-concrete 0) [0 0])))

(defn check-with-pattern [pat loc]
  (= ["X" "M" "A" "S"] (get-with-pattern pat loc)))

(comment
  (check-with-pattern (get patterns-concrete 0) [0 0]))

(def candidates
  (for [r (range (count twodarr))
        c (range (count (first twodarr)))
        pattern patterns-concrete]
    [pattern [r c]]))

(def part1
  (count (filter (partial apply check-with-pattern) candidates)))

