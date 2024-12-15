(ns aoc2024.day15
  "day 15"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def day15-input
  (str/trim (slurp "input/15.txt")))

(def sample
  "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

(def use-sample false)

(def input
  (if use-sample
    sample
    day15-input))

(def initial-map
  (->> (str/split input #"\n\n")
       (first)
       (str/trim)
       (str/split-lines)
       (map-indexed (fn [r row] (map-indexed (fn [c chr] [[r c] chr]) (seq row))))
       (apply concat)
       (#(into {} %))))

(defn find-robot [curr-map] (first (filter #(= (curr-map %) \@) (keys curr-map))))

(def initial-robot (find-robot initial-map))

(def moves
  (->> (str/split input #"\n\n")
       (second)
       (str/trim)
       (str/split-lines)
       (apply concat)))

(def initial-state
  {:map initial-map
   :robot initial-robot})

(defn offset-from-move [move]
  (match move
    \< [0 -1]
    \> [0 1]
    \^ [-1 0]
    \v [1 0]))

(defn push-into [state loc offset]
  (let [nextloc (map + loc offset)
        me (get-in state [:map loc])
        ;; _ (println nextloc)
        ;; _ (println me)
        ]
    (match (get-in state [:map nextloc] \#)
      \. {:next-state (-> state
                          (update-in [:map nextloc] (constantly me))
                          (update-in [:map loc] (constantly \.)))
          :ok true}
      \# {:next-state state
          :ok false}
      \O (let [{next-state :next-state
                ok :ok} (push-into state nextloc offset)]
           (if ok
             ;; {:next-state (-> next-state
             ;;                  (update-in [:map nextloc] (constantly me))
             ;;                  (update-in [:map loc] (constantly \.)))
             (recur next-state loc offset)
             {:next-state state :ok false})))))

(defn step [state move]
  (let [robot (:robot state)
        ;; _ (println robot)
        offset (offset-from-move move)
        ;; _ (println offset)
        {next-state :next-state
         ok :ok} (push-into state robot (offset-from-move move))]
    (if ok
      (assoc next-state :robot (map + robot offset))
      next-state)))

(def final-state (reduce step initial-state moves))

(def max-row (->> (keys initial-map)
                  (map first)
                  (apply max)))

(def max-col (->> (keys initial-map)
                  (map second)
                  (apply max)))

(defn print-state [state]
  (doseq [r (range (+ 1 max-row))]
    (doseq [c (range (+ 1 max-col))]
      (print (get-in state [:map [r c]])))  (print "\n")))

(print-state final-state)

(def part1 (->> (keys (:map final-state))
                (filter #(= \O (get-in final-state [:map %])))
                (map (fn [[r c]] (+ (* 100 r) c)))
                (reduce +)))

(printf "part1: %d%n" part1)
