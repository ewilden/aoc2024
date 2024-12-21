(ns aoc2024.day18
  "day 18"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def day18-input
  (str/trim (slurp "input/18.txt")))

(def sample
  "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")

(def use-sample false)

(def input (if use-sample sample day18-input))
(def max-coord (if use-sample 6 70))
(def initial-take (if use-sample 12 1024))
(def target [max-coord max-coord])
(defn parse-line [line]
  (let [[_ x y] (re-matches #"(\d+),(\d+)" line)]
    [(parse-long x) (parse-long y)]))

(def incoming (map parse-line (str/split-lines input)))

(defn occupied-after [t] (into #{} (take t incoming)))

(defn draw-occupied [xs]
  (doseq [y (range (+ 1 max-coord))]
    (doseq [x (range (+ 1 max-coord))]
      (if (xs [x y])
        (print "#")
        (print ".")))
    (println "")))

(def inf 1000000000000)
(def all-states (for [x (range (+ 1 max-coord))
                      y (range (+ 1 max-coord))]
                  [x y]))
(def initial-state [0 0])

(def initial-costs (assoc (zipmap
                           all-states (repeat inf)) initial-state 0))

(def initial-dijkstra
  {:costs initial-costs
   :prevs {}
   :unvisited (into #{} all-states)})

(def all-offsets [[0 1] [1 0] [-1 0] [0 -1]])

(defn in-grid [[x y]]
  (and
   (>= x 0)
   (>= y 0)
   (<= x max-coord)
   (<= y max-coord)))

(defn edges-from-at [t]
  (let [occupied (occupied-after t)]
    (fn [node] (->> (map (partial map +) all-offsets (repeat node))
                    (filter in-grid)
                    (filter #(not (occupied %)))
                    (map (fn [loc] {:next-state loc :cost 1}))))))

(defn dijkstra [edges-from curr]
  (let [{costs :costs
         unvisited :unvisited
         prevs :prevs} curr]
    (if (empty? unvisited)
      curr
      (let [node (apply min-key costs unvisited)
            unvisited (clojure.set/difference unvisited #{node})
            neighbors (->> (edges-from node)
                           (filter #(unvisited (% :next-state))))
            {costs :costs prevs :prevs} (reduce
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
        (recur edges-from {:costs costs
                           :prevs prevs
                           :unvisited unvisited})))))

(def dijkstra-results (dijkstra (edges-from-at initial-take) initial-dijkstra))
(get-in dijkstra-results [:costs [1 0]])

;; (draw-occupied (occupied-after initial-take))

(def part1 (get-in dijkstra-results [:costs target]))
(println "part1:" part1)

(defn ufind [[parent size] loc]
  (if (= (parent loc) loc)
    loc
    (recur [parent size] (parent loc))))

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
       (assoc size larger (+ (size larger) (size smaller)))])))

(def coordrange (range (+ 1 max-coord)))

(def left-edgeset
  (map (fn [y] [-1 y]) coordrange))
(def bottom-edgeset
  (map (fn [x] [x (+ 1 max-coord)]) coordrange))
(def top-edgeset
  (map (fn [x] [x -1]) coordrange))
(def right-edgeset
  (map (fn [y] [(+ 1 max-coord) y]) coordrange))

(def initial-left-union-parent (->> (concat left-edgeset bottom-edgeset)
                                    (cons :left)
                                    (#(zipmap % (repeat :left)))))

(def initial-right-union-parent (->> (concat right-edgeset top-edgeset)
                                     (cons :right)
                                     (#(zipmap % (repeat :right)))))

(def initial-union-state
  [(->> (zipmap all-states all-states)
        (concat initial-left-union-parent)
        (concat initial-right-union-parent)
        (into {}))
   (->> (zipmap all-states (repeat 1))
        (concat (-> initial-left-union-parent
                    (update-vals (constantly (- (count initial-left-union-parent) 1)))
                    (assoc :left (count initial-left-union-parent))))
        (concat (-> initial-right-union-parent
                    (update-vals (constantly (- (count initial-right-union-parent) 1)))
                    (assoc :right (count initial-left-union-parent))))
        (into {}))])

(defn neighborhood [occupied incoming]
  (filter occupied
          (for [xoff [-1 0 1]
                yoff [-1 0 1]]
            (map + [xoff yoff] incoming))))

(defn union-from-incoming [state occupied incoming]
  (let [occupied (conj occupied incoming)
        nbrs (neighborhood occupied incoming)
        state (reduce
               (fn [state nbr]
                 (union state nbr incoming)) state nbrs)] [state occupied]))

(defn find-first-blocking [state occupied incoming]
  (let [head (first incoming)
        tail (rest incoming)
        [state occupied] (union-from-incoming state occupied head)]
    (if (nil? head)
      (do
        (println state)
        (println occupied)
        nil)
      (if (= (ufind state :left) (ufind state :right))
        head
        (recur state occupied tail)))))

(def initial-occupied (into #{}
                            (concat
                             left-edgeset bottom-edgeset right-edgeset top-edgeset)))

(def part2 (find-first-blocking initial-union-state initial-occupied incoming))

(println "part2:" part2)
