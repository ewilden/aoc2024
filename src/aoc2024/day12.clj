(ns aoc2024.day12
  "day 12"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def day12-input
  (str/trim (slurp "input/12.txt")))

;; (def sample
;; "AAAA
;; BBCD
;; BBCC
;; EEEC")
(def sample
  "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(def grid
  (->>
   day12-input
   ;; sample
   (str/split-lines)
   (map-indexed (fn [r row] (map-indexed (fn [c chr] [[r c] chr]) (seq row))))
   (apply concat)
   (#(into {} %))))

(def offsets
  [[1 0]
   [0 1]
   [-1 0]
   [0 -1]])

(defn neighbors [loc]
  (->>
   (map #(map + % loc) offsets)
   (filter #(some? (grid %)))))

(defn internal-edges-from [loc]
  (->> (neighbors loc)
       (filter #(= (grid loc) (grid %)))))

(internal-edges-from [0 3])

(def initial-state
  [(into {} (map (fn [k] [k k]) (keys grid)))
   (into {} (map (fn [k] [k 1]) (keys grid)))
   (count grid)])

(defn ufind [[parent size n] loc]
  (if (= (parent loc) loc)
    loc
    (recur [parent size n] (parent loc))))

(defn union [state p q]
  (let [root-p (ufind state p)
        root-q (ufind state q)
        [parent size n] state
        root-p-is-smaller (< (size root-p) (size root-q))
        smaller (if root-p-is-smaller
                  root-p
                  root-q)
        larger (if root-p-is-smaller
                 root-q
                 root-p)]
    (if (= root-p root-q)
      state
      [(assoc parent smaller larger)
       (assoc size larger (+ (size larger) (size smaller)))
       (- n 1)])))

(def unioned
  (->> (keys grid)
       (reduce
        (fn
          [state loc]
          (reduce
           (fn
             [[parent size n] loc']
             (union [parent size n] loc loc'))
           state
           (internal-edges-from loc)))
        initial-state)))

(def areas (group-by #(ufind unioned %) (keys grid)))

(defn perimeter [area]
  (->> (areas area)
       (map (fn [loc] (- 4 (count (internal-edges-from loc)))))
       (reduce +)))

(defn cost [area]
  (* (perimeter area) (count (areas area))))

(def part1
  (->> (keys areas)
       (map cost)
       (reduce +)))

(printf "part1: %d%n" part1)

(defn region [loc] (ufind unioned loc))

(defn edge-exits-region [a b]
  (not= (grid a) (grid b)))

(defn score-edge [a b]
  (let [offset (map - b a)
        [ox oy] offset
        rotated-offset [(- oy) ox]
        a' (map + rotated-offset a)
        b' (map + offset a')]
    (if (edge-exits-region a b)
      (if (= (grid a) (grid a'))
        (if (not (edge-exits-region a' b'))
          1
          0)
        1)
      0)))

(defn perimeter-part2 [area]
  (->> (areas area)
       (map (fn [loc]
              (->> (map (fn [off] (map + loc off)) offsets)
                   (map #(score-edge loc %))
                   (reduce +))))
       (reduce +)))

(defn cost-part2 [area]
  (* (perimeter-part2 area) (count (areas area))))

(println (perimeter-part2 (region [0 0])))
(println (count (areas (region [0 0]))))
(println (cost-part2 (region [0 0])))

(def part2
  (->> (keys areas)
       (map cost-part2)
       (reduce +)))

(printf "part2: %d%n" part2)
