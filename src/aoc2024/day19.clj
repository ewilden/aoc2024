(ns aoc2024.day19
  "day 19"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def day19-input
  (str/trim (slurp "input/19.txt")))

(def sample "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(def use-sample false)

(def input (if use-sample sample day19-input))

(def patterns
  (let [[pats _] (str/split input #"\n\n")
        pats (str/split pats #", ")]
    pats))

(def designs
  (let [[_ designs] (str/split input #"\n\n")]
    (str/split-lines designs)))

(defn build-trie [seed & kvs]
  (reduce
   (fn [trie [k v]]
     (assoc-in trie (concat k [:val]) v))
   seed
   (partition 2 kvs)))

(defn prefix-match [target trie]
  (when (seq target)
    (when-let [node (trie (first target))]
      (let [h (:val node)
            tl (prefix-match (rest target) node)]
        (if h
          (cons h tl)
          tl)))))

(def trie (apply build-trie {} (interleave patterns patterns)))

(defn munch-prefix-of-design [design]
  (->> (prefix-match design trie)
       (map #(subs design (count %)))))

(defn design-is-possible [designs]
  (if (some (partial = "") designs) true
      (let [designs (into #{} (mapcat munch-prefix-of-design designs))]
        (if (empty? designs)
          false
          (recur designs)))))

(def part1
  (->> designs
       (filter #(design-is-possible #{%}))
       (count)))

(println "part1:" part1)

(defn count-possibilities [sum designs]
  (let [total (count designs)
        designs (remove (partial = "") designs)
        num-empty (- total (count designs))
        sum (+ sum num-empty)
        designs (mapcat munch-prefix-of-design designs)]
    (if (empty? designs)
      sum 
      (recur sum designs))))

(def foo
  (memoize
    (fn [design]
      (if (= "" design)
        1 
        (let [designs (munch-prefix-of-design design)]
          (if (empty? designs)
            0
            (let [total (count designs)
                  designs (remove ( partial = "") designs) 
                  num-empty (- total (count designs)) ] (apply + num-empty (map foo designs)))))))))

(def part2 (->> designs 
                (map foo)
                (reduce +)))

(println "part2:" part2)
