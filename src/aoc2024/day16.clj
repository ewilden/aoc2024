(ns aoc2024.day16
  "day 16"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def day16-input
  (str/trim (slurp "input/16.txt")))

(def sample
  "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")

(def use-sample
  false)

(def input
  (if use-sample
    sample
    day16-input))

(def grid
  (->> (str/split-lines input)
       (map-indexed (fn [r row] (map-indexed (fn [c chr] [[r c] chr]) (seq row))))
       (apply concat)
       (#(into {} %))))

(def start (first (filter #(= (grid %) \S) (keys grid))))
(def end (first (filter #(= (grid %) \E) (keys grid))))
(def start-offset [0 1])
(defn rotate-left [[r c]] [(- c) r])
(rotate-left start-offset)
(defn rotate-right [offset] (nth (iterate rotate-left offset) 3))
(def all-offsets (map #(nth (iterate rotate-left start-offset) %) (range 4)))
(defn rotations-to [off1 off2]
  (let [via-left (keep-indexed
                  (fn [i off] (when (= off off2) i))
                  (iterate rotate-left off1))
        via-right (keep-indexed
                   (fn [i off] (when (= off off2) i))
                   (iterate rotate-right off1))] (min (first via-left) (first via-right))))

(def initial-state
  {:pos start
   :facing start-offset})

(defn allowed-to-enter [loc]
  (contains? #{\. \E \S} (grid loc)))

(defn edges-from [state]
  (->> (take 4 (iterate rotate-left (:facing state)))
       (drop 1)
       (map (fn [facing] (assoc state :facing facing)))
       (map (fn [next-state] {:next-state next-state
                              :cost (*
                                     1000
                                     (rotations-to
                                      (:facing next-state)
                                      (:facing state)))}))
       (#(conj % {:next-state
                  (update state :pos (partial map + (:facing state)))
                  :cost 1}))
       (filter #(allowed-to-enter (get-in % [:next-state :pos])))))

(def inf 1000000000)
(defn for-all-offsets [loc]
  (for [facing all-offsets]
    {:pos loc :facing facing}))

(def all-states (mapcat
                 (fn [loc] (for [facing all-offsets]
                             {:pos loc :facing facing}))
                 (keys grid)))
(def initial-costs (assoc (zipmap
                           all-states (repeat inf)) initial-state 0))

(def initial-dijkstra
  {:costs initial-costs
   :unvisited (into #{} all-states)})

(defn dijkstra [curr]
  (let [{costs :costs
         unvisited :unvisited} curr]
    (if (empty? unvisited)
      curr
      (let [node (apply min-key costs unvisited)
            unvisited (clojure.set/difference unvisited #{node})
            neighbors (->> (edges-from node)
                           (filter #(unvisited (% :next-state))))
            costs (reduce
                   (fn [costs {state :next-state cost :cost}]
                     (update costs state
                             (fn [old-cost]
                               (min old-cost (+ (costs node) cost)))))
                   costs
                   neighbors)]
        (recur {:costs costs
                :unvisited unvisited})))))
(def dijkstra-results (dijkstra initial-dijkstra))
(def end-costs (select-keys (:costs dijkstra-results) (for-all-offsets end)))
(def part1 (apply min (vals end-costs)))

(printf "part1: %d%n" part1)
