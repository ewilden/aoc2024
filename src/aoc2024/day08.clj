(ns aoc2024.day08
  "day 8"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])
(require '[clojure.math :refer [ceil floor]])

(def day08-input
  (slurp "input/08.txt"))

(def grid
  (->> day08-input
       (str/split-lines)
       (mapv #(mapv identity (seq %)))))

(def num-rows (count grid))
(def num-cols (count (first grid)))

(def grid-map
  (->> grid 
       (map-indexed (fn [r row] (map-indexed (fn [c item] [[r c] item]) row)))
       (apply concat)
       (filter #(not= \. (second %)))
       (into {})
  ))

(def by-freq (update-vals (group-by second grid-map)
                          #(map first %)
                  ))

(defn antinodes [chr] 
  (let [locs (by-freq chr)
        antinode-locs (remove nil? (for [a locs
                                         b locs]
                                     (when (not= a b)
                                       (let [delta (map - b a)
                                             bside (map + b delta)
                                             aside (map - a delta)]
                                         [aside bside]))))]
    (->> antinode-locs
         (apply concat)
         (filter (fn [[r c]] (and 
                               (>= r 0)
                               (< r num-rows)
                               (>= c 0)
                               (< c num-cols)))))))

(def part1
  (->> (map antinodes (keys by-freq))
       (apply concat)
       (into #{})
       (count)))

(defn numer [n] (if (ratio? n) (numerator n) n))
(defn denom [n] (if (ratio? n) (denominator n) 1))

(comment r = mc + b 
         b = r - mc
         c = (r - b) / m)

(def maxcoord (- (count grid) 1))

(defn points-in-line [ [r1 c1] [r2 c2]]
  (let [m (/ (- r2 r1) (- c2 c1))
        ;; bigger (max (numer rat) (denom rat))
        ;; smaller (max (numer rat) (denom rat))
        b (- r1  (* m c1))
        pts-left ( ->> (iterate (fn [[r c]] [(- r (numer m)) (- c (denom m))]) [r1 c1]) 
                       (take-while (fn [dims] (every? #(and (>= % 0) (<= % maxcoord)) dims))))
        pts-right ( ->> (iterate (fn [[r c]] [(+ r (numer m)) (+ c (denom m))]) [r1 c1]) 
                       (take-while (fn [dims] (every? #(and (>= % 0) (<= % maxcoord)) dims))))
        ;; r_when_c_0 b 
        ;; r_when_c_maxcoord (+ b (* m maxcoord))
        ;; c_when_r_0 (/ (- b) m)
        ;; c_when_r_maxcoord (/ (- maxcoord b) m)
        ;; min_r (max 0 (long (ceil (min r_when_c_0 r_when_c_maxcoord))))
        ;; max_r (min maxcoord (long (floor (max r_when_c_0 r_when_c_maxcoord))))
        ;; min_c (max 0 (long (ceil (min c_when_r_0 c_when_r_maxcoord))))
        ;; max_c (min maxcoord (long (floor (max c_when_r_0 c_when_r_maxcoord))))
        ;; r_below (/ (- r1 min_r) (abs (numer m)))
        ;; r_above (/ (- max_r r1) (abs (numer m)))
        ;; c_below (/ (- c1 min_c) (abs (denom m)))
        ;; c_above (/ (- max_c c1) (abs (denom m)))
        ]
    (concat pts-left pts-right)
      ;; (print [r_below r_above c_below c_above])
      ;; (min (+ 1 (long (floor r_below)) (long (floor r_above)))
      ;;      (+ 1 (long (floor c_below)) (long (floor c_above))))
    )
  )

(def part2
  (->> (map second by-freq)
       (map (fn [locs] (for [a locs b locs] (when (not= a b) (points-in-line a b)))))
       (map (partial remove nil?))
       (apply concat)
       (apply concat)
       (into #{})
       (count)
       ))

