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

(def part2-corner-patterns
  [[:tl :bl]
   [:tl :tr]
   [:tr :br]
   [:bl :br]])

(defn to-abstract-part2 [corner-pat]
  (map (fn [corner] (case corner
                      :tl [:asc :asc]
                      :bl [:desc :asc]
                      :tr [:asc :desc]
                      :br [:desc :desc])) corner-pat))

(comment
  (to-abstract-part2 [:tl :bl]))

(defn to-concrete-part2 [abstpat]
  (case abstpat
    :asc [0 1 2]
    :desc [0 -1 -2]))

(comment
  (map (fn [abstpats] (map to-concrete-part2 abstpats))
       (to-abstract-part2 [:tl :bl])))

(defn zip [a b]
  (map vector a b))

(def coord-offsets
  (->> part2-corner-patterns
       (map to-abstract-part2)
       (map #(map (fn [abstpats] (map to-concrete-part2 abstpats)) %))
       (map (fn [ls] (map #(apply zip %) ls)))
       ;; (map (fn [[roffs coffs]] (map vector roffs coffs)))
       ;; (map (fn [coordpats] (map (fn [rpat cpat] (map vector rpat cpat)) coordpats)))
       ))
(def candidates-part2
  (for [offsets coord-offsets
        r (range (count twodarr))
        c (range (count (first twodarr)))]
    (map (fn [offsets] (map (fn [[roff coff]] [(+ r roff) (+ c coff)]) offsets)) offsets)))

(comment
  (nth candidates-part2 100))

(defn extract-candidate-part2 [candidate]
  (map (fn [coords]
         (map #(get-in twodarr %) coords)) candidate))

(comment
  (extract-candidate-part2 (nth candidates-part2 100)))

(defn check-candidate-part2 [candidate]
  (->> (extract-candidate-part2 candidate)
       (every? (partial = ["M" "A" "S"]))))

(comment (check-candidate-part2 (nth candidates-part2 100)))

(comment (map extract-candidate-part2 candidates-part2))

(def part2 (count (filter check-candidate-part2 candidates-part2)))

