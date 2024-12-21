(ns aoc2024.day20
  "day 20"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def day20-input
  (str/trim (slurp "input/20.txt")))

(def sample "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############")

(def use-sample
  true
  ;; spacing
  )

(def input (if use-sample sample day20-input))

(def grid
  (->> (str/split-lines input)
       (map-indexed (fn [r row] (map-indexed (fn [c chr] [[r c] chr]) (seq row))))
       (apply concat)
       (#(into {} %))))

(def num-cols (+ 1 (->> (keys grid)
                        (map (fn [[r c]] c))
                        (apply max))))
(def num-rows (+ 1 (->> (keys grid)
                        (map (fn [[r c]] r))
                        (apply max))))

(def inf 1000000000000)
(def all-states (keys grid))

(defn find-elem [chr] (->> (keys grid)
                           (filter #(= (grid %) chr))
                           (first)))

(def start (find-elem \S))
(def end (find-elem \E))

(def initial-costs (assoc (zipmap
                           all-states (repeat inf)) start 0))

(def initial-dijkstra
  {:costs initial-costs
   :prevs {}
   :unvisited (into #{} all-states)})

(def all-offsets [[0 1] [1 0] [-1 0] [0 -1]])

(defn edges-from [node]
  (->> (map (partial map +) all-offsets (repeat node))
       (filter grid)
       (filter #(not= \# (grid %)))
       (map (fn [loc] {:next-state loc :cost 1}))))

(defn neighborhood [node]
  (->> (map (partial map +) all-offsets (repeat node))
       (filter grid)))

(defn dijkstra [curr]
  (let [{costs :costs
         unvisited :unvisited
         prevs :prevs} curr]
    (if (empty? unvisited)
      curr
      (let [node (apply min-key costs unvisited)
            unvisited (clojure.set/difference unvisited #{node})
            neighbors (->> (edges-from node)
                           (filter #(unvisited (% :next-state))))
            {costs :costs prevs :prevs}
            (reduce
             (fn [{costs :costs prevs :prevs} {state :next-state cost :cost}]
               (let [old-cost (costs state)
                     new-cost (+ (costs node) cost)]
                 (if (< new-cost old-cost)
                   {:costs (assoc costs state new-cost)
                    :prevs (assoc prevs state #{node})}
                   (if (= new-cost old-cost)
                     {:costs costs
                      :prevs (update prevs state #(conj % node))}
                     {:costs costs :prevs prevs}))))
             {:costs costs :prevs prevs}
             neighbors)]
        (recur {:costs costs
                :prevs prevs
                :unvisited unvisited})))))

(def dijkstra-results (dijkstra initial-dijkstra))

(def cost-without-cheating (get-in dijkstra-results [:costs end]))

(defn initial-cheatstate [total-steps]
  {:pos start
   :cheated nil
   :steps-remaining total-steps})

(defn cheat-edges-from [state]
  (let [no-cheat (into #{} (map :next-state (edges-from (:pos state))))
        need-cheat (into #{} (remove no-cheat (neighborhood (:pos state))))
        next-steps (- (:steps-remaining state) 1)]
    (match (:cheated state)
      nil (concat
           (map
            (fn [pos]
              {:pos pos :cheated nil :steps-remaining next-steps})
            no-cheat)
           (map
            (fn [pos]
              {:pos pos :cheated {:start pos} :steps-remaining next-steps})
            no-cheat)
           (map
            (fn [pos]
              {:pos pos :cheated {:start pos} :steps-remaining next-steps})
            need-cheat))
      {:start s :end e}
      (map
       (fn [pos]
         {:pos pos :cheated (:cheated state) :steps-remaining next-steps})
       no-cheat)
      {:start s}
      (map
       (fn [pos]
         {:pos pos
          :cheated {:start s :end pos}
          :steps-remaining next-steps})
       (concat no-cheat need-cheat)))))

(def reaches-end
  (memoize
   (fn [state]
     (if (= end (:pos state))
       [(:cheated state)]
       (if (= 0 (:steps-remaining state))
         nil
         (->> (cheat-edges-from state)
              (map reaches-end)
              (apply concat)))))))

(println (reaches-end (initial-cheatstate (- cost-without-cheating 64))))



