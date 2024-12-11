(ns aoc2024.day09
  "day 9"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

;; (def day09-input
;;   (str/trim (slurp "input/09-sample.txt")))
(def day09-input
  (str/trim (slurp "input/09.txt")))

(def files
  (->> (seq day09-input)
       (map (fn [chr] (Character/digit chr 10)))
       (map vector (interpose :space (range)))
       (mapcat (fn [[index length]] (repeat (bigint length) index)))))

(def compacted-length (count (filter (partial not= :space) files)))

(def reduce-to-fill (reduce
                     (fn [[output reved] item]
                       (let [reved (drop-while (partial = :space) reved)]
                         (match [item (first reved)]
                           [:space n] [(conj output n) (rest reved)]
                           [n _] [(conj output n) reved])))
                     [[] (reverse files)]
                     files))

(def part1
  (->> (first reduce-to-fill)
       (take compacted-length)
       (map-indexed (fn [blockpos fileno] (* blockpos fileno)))
       (reduce +)))

(printf "part1: %d%n" part1)

(def files-part2
  (->> (seq day09-input)
       (map (fn [chr] (Character/digit chr 10)))
       (map vector (interpose :space (range)))
       (reduce (fn [[size-so-far output] [fileno size]]
                 [(+ size-so-far size)
                  (conj output
                        {:block-index size-so-far
                         :fileno fileno
                         :size size})])

               [0 []])
       (second)))

(def just-files-sorted
  (->> files-part2
       (filter #(not= :space (:fileno %)))
       (sort-by :fileno)))

(def files-grouped-part2
  (group-by :fileno files-part2))

(def spaces
  (->> (:space files-grouped-part2)))

(defn fits [file space] (>= (:size space) (:size file)))
(defn filter-fitting [file spaces] (filter (partial fits file) (filter #(<= (:block-index %) (:block-index file)) spaces)))
(defn first-fit [file spaces] (first (sort-by :block-index (filter-fitting file spaces))))
(defn shrink-space [amount space]
  (when (> (:size space) amount)
    {:block-index (+ (:block-index space) amount)
     :fileno :space
     :size (- (:size space) amount)}))

(def reduce-to-fill-part2
  (reduce
   (fn [[output spaces] file]
     (let [fitter (first-fit file spaces)]
       (if (nil? fitter)
         [(conj output file) spaces]
         [(conj output (assoc file :block-index (:block-index fitter)))
          (let [shrunk (shrink-space (:size file) fitter)
                spaces' (disj spaces fitter)]
            (if (nil? shrunk)
              spaces'
              (conj spaces' shrunk)))])))
   [#{} (into #{} spaces)]
   (reverse just-files-sorted)))

(->>
 reduce-to-fill-part2
 (first)
 (sort-by :block-index))

(def part2 (->> (first reduce-to-fill-part2)
                (map
                 (fn [file]
                   (->>
                    (map (partial + (:block-index file)) (range (:size file)))
                    (map (partial * (:fileno file)))
                    (reduce +))))

                (reduce +)))

(printf "part2: %d%n" part2)
