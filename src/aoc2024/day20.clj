(ns aoc2024.day20
  "day 20"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])
(require '[clojure.data.priority-map :refer [priority-map]])

(def day20-input
  (str/trim (slurp "input/20.txt")))

(def sample
  "###############
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
  ;; true
  false
  ;; spacer
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

(def all-offsets [[0 1] [1 0] [-1 0] [0 -1]])

(def all-states
  (interleave
   (filter
    #(not= (grid %) \#)
    (map (fn [loc] {:cheated false :loc loc}) (keys grid)))
   (map (fn [loc] {:cheated true :loc loc}) (keys grid))))

(defn neighbors-unchecked [loc]
  (into #{} (filter grid (map #(map + % loc) all-offsets))))

(defn edges-from-when [disallowed-cheats]
  (fn [state]
    (map (fn [state] {:next-state state :cost 1})
         (if (:cheated state)
           (map (fn [loc] {:cheated true :loc loc})
                (filter
                 #(not= (grid %) \#)
                 (neighbors-unchecked (:loc state))))
           (concat
            (map (fn [loc] {:cheated false :loc loc})
                 (filter
                  #(not= (grid %) \#)
                  (neighbors-unchecked (:loc state))))
            (map (fn [loc] {:cheated true :loc loc})
                 (filter #(not (disallowed-cheats %))
                         (neighbors-unchecked (:loc state)))))))))

(defn find-elem [chr] (->> (keys grid)
                           (filter #(= (grid %) chr))
                           (first)))

(def start (find-elem \S))
(def end (find-elem \E))

(def initial-costs (->> (assoc (zipmap
                                all-states (repeat inf)) {:cheated false :loc start} 0)
                        (into (priority-map))))
(defn update-cost [dijkstra-state
                   {curr :curr
                    edge-cost :edge-cost
                    nbr :nbr}]
  (let [is-unvisited (get-in dijkstra-state [:unvisited nbr])
        map-choice (if is-unvisited :unvisited (throw (Exception. "unreachable")))
        old-nbr-cost (get-in dijkstra-state [map-choice nbr])
        curr-cost (get-in dijkstra-state [:costs curr])
        new-nbr-cost (+ edge-cost curr-cost)]
    (if
     (< new-nbr-cost old-nbr-cost)
      (do
      ;; (println "updated cost of" nbr "to" new-nbr-cost)
        (-> dijkstra-state
            (assoc-in [map-choice nbr] new-nbr-cost)
            (assoc-in [:prevs nbr] #{curr})))
      (if
       (= new-nbr-cost old-nbr-cost)
        (do
          ;; (println "added new prev" curr "for" nbr)
          (-> dijkstra-state
              (update-in [:prevs nbr] #(conj % curr))))

        dijkstra-state))))

(def initial-dijkstra
  {:costs initial-costs
   :prevs {}
   :unvisited initial-costs})

(defn dijkstra [edges-from curr]
  (let [{costs :costs
         unvisited :unvisited} curr]
    (if (not (and (unvisited {:cheated false :loc end})
                  (unvisited {:cheated true :loc end})))
      curr
      (let [[node node-cost] (peek unvisited)
            unvisited (pop unvisited)
            costs (assoc costs node node-cost)
            curr (merge curr {:costs costs :unvisited unvisited})
            neighbors (->> (edges-from node)
                           (filter #(unvisited (% :next-state))))
            curr
            (reduce
             (fn [curr {state :next-state cost :cost}]
               (update-cost curr {:curr node :edge-cost cost :nbr state}))
             curr
             neighbors)]
        (recur edges-from curr)))))

(def dijkstra-results (dijkstra (edges-from-when (constantly true)) initial-dijkstra))

(:prevs dijkstra-results)

(def cost-without-cheating (get-in dijkstra-results [:costs {:cheated false :loc end}]))
(println "cost without cheating:" cost-without-cheating)

(defn find-cheat [prevs curr]
  (let [next-node (first (prevs curr))]
    (if (not= (:cheated curr) (:cheated next-node))
      (do
        ;; (println "curr" curr "next" next-node)
        (:loc curr))
      (recur prevs next-node))))

(def cost-with-cheating
  (get-in (dijkstra (edges-from-when #{}) initial-dijkstra) [:costs {:cheated true :loc end}]))

(defn cheats-under-threshold [threshold already-used]
  (let [results (dijkstra (edges-from-when already-used) initial-dijkstra)
        cost (get-in results [:costs {:cheated true :loc end}])
        ;; _ (println "cost:" cost)
        cheatloc (find-cheat (:prevs results) {:cheated true :loc end})
        ;; _ (println "cheat:" cheatloc)
        already-used (conj already-used cheatloc)
        ;; _ (println "already-used:" already-used)
        ]
    (if (< threshold cost)
      nil
      (do
        (println cost)
        (cons cheatloc (cheats-under-threshold threshold already-used))))))

(def threshold-diff
  (if use-sample 20 100))

;;
;;
;; (def part1
;;   (->> (cheats-under-threshold (- cost-without-cheating threshold-diff) #{})
;;        ;; (map #(do (println %) %))
;;        (count)))
;;
;; (println "part1:" part1)

(defn manhattan [[ra ca] [rb cb]]
  (+ (abs (- ra rb)) (abs (- ca cb))))

(defn edges-from-when-part2 [disallowed-cheats]
  (fn [state]
    (if (:cheated state)
      (->> (neighbors-unchecked (:loc state))
           (filter #(not= (grid %) \#))
           (map (fn [loc] {:next-state {:cheated true :loc loc} :cost 1})))
      (concat
       (->> (neighbors-unchecked (:loc state))
            (filter #(not= (grid %) \#))
            (map (fn [loc] {:next-state {:cheated false :loc loc} :cost 1})))
       (->> (for [roff (range -20 20)
                  coff (range -20 20)] [roff coff])
            (filter #(not= % [0 0]))
            (filter #(>= 20 (manhattan [0 0] %)))
            (map #(map + % (:loc state)))
            (filter grid)
            (filter #(not= (grid %) \#))
            (filter #(not (disallowed-cheats #{(:loc state) %})))
            (map (fn [loc] {:next-state {:cheated true :loc loc} :cost (manhattan loc (:loc state))})))))))

(defn find-cheats-part2-aux [prevs currs acc]
  (let [currs (into #{} (filter :cheated currs))
        ;; _ (println "currs" currs)
        edges (into #{} (mapcat (fn [curr] (map (fn [prev] #{prev curr}) (prevs curr))) currs))
        edges (into #{} (filter (fn [edge] (let [[a b] (into [] edge)] (not= (:cheated a) (:cheated b))))) edges)
        edges (into #{} (map (fn [edge] (into #{} (map :loc edge))) edges))
        ;; _ (println edges)
        acc (clojure.set/union acc edges)
        nexts (into #{} (mapcat prevs currs))
        ]
    (if (empty? currs)
      acc 
      (recur prevs nexts acc)
      )))

(defn find-cheats-part2 [prevs curr]
  (find-cheats-part2-aux prevs #{curr} #{}))

(def cost-with-cheating-part2
  (get-in (dijkstra (edges-from-when-part2 #{}) initial-dijkstra)
          [:costs {:cheated true :loc end}]))

(defn cheats-under-threshold-part2 [threshold already-used]
  (let [results (dijkstra (edges-from-when-part2 already-used) initial-dijkstra)
        cost (get-in results [:costs {:cheated true :loc end}])
        prev-already-used already-used
        ;; _ (println "cost:" cost)
        cheatlocs (find-cheats-part2 (:prevs results) {:cheated true :loc end})
        ;; _ (println "cheat:" cheatloc)
        already-used (clojure.set/union already-used cheatlocs)
        ;; _ (println "already-used:" already-used)
        ]
    (if (< threshold cost)
      prev-already-used
      (do
        (println cost)
        (recur threshold already-used)))))

(def threshold-diff-part2
  (if use-sample 74 100))

(def part2
  (->> (cheats-under-threshold-part2
        (- cost-without-cheating threshold-diff-part2) #{})
       (map #(do (println %) %))
       (count)))

(println "part2:" part2)
