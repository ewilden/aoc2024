(ns aoc2024.day05
  "day 5"
  (:require
   [clojure.string :as str]))

(def day05-input
  (slurp "input/05.txt"))
;; (def day05-input
;; (slurp "input/05-sample.txt"))

(def updates
  (->>
   (str/split-lines
    (get (str/split day05-input #"\n\n") 1))
   (map #(str/split % #","))
   (map (partial mapv parse-long))))
(def rules
  (get (str/split day05-input #"\n\n") 0))

(def rules-map
  (->> (str/split-lines rules)
       (map #(str/split % #"\|"))
       (map #(map parse-long %))
       (reduce (fn [acc [l r]] (let [prev (get acc l)]
                                 (assoc acc l (conj (or prev #{}) r)))) {})))

(defn scan-update [update]
  (reduce (fn [seen n] (let [must_succeed (get rules-map n)
                             intersects (seq (clojure.set/intersection must_succeed seen))]
                         (if intersects
                           (reduced [:failed intersects n])
                           (conj seen n)))) #{} update))

(comment
  (scan-update (first updates)))

(def correct-updates (filter #(not= :failed (get (scan-update %) 0)) updates))

(def part1 (->> correct-updates
                (map #(get % (quot (count %) 2)))
                (reduce +)))

(scan-update [75 97 47 61 53])

(def incorrect-updates (filter #(= :failed (get (scan-update %) 0)) updates))

(defn swap [ls a b]
  (replace {:to-swap b} (replace {b a} (replace {a :to-swap} ls))))

(defn scan-and-sort [update]
  (let [result (scan-update update)] (if (= :failed (first result))
                                       (scan-and-sort (swap update (first (second result)) (nth result 2))) update)))

(scan-and-sort [75 97 47 61 53])

(def part2 (->> incorrect-updates
                (map scan-and-sort)
                (map #(get % (quot (count %) 2)))
                (reduce +)))
