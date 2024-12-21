(ns aoc2024.day17
  "day 17"
  (:require
   [clojure.string :as str]))

(require '[clojure.core.match :refer [match]])

(def day17-input
  (str/trim (slurp "input/17.txt")))

(def sample "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0")

(def use-sample
  false)
(def input (if use-sample sample day17-input))

(defn parse-reg [line]
  (let [[_ raw-reg] (re-matches #"Register .: (\d+)" line)]
    (parse-long raw-reg)))

(defn parse-prog [line]
  (let [[_ nums] (re-matches #"Program: ((\d,?)+)" line)]
    (map parse-long (str/split nums #","))))

(def parsed-input
  (let [[raw-reg raw-prog] (str/split input #"\n\n")
        [a b c] (map parse-reg (str/split-lines raw-reg))
        parsed-prog (into [] (parse-prog raw-prog))]
    {:state {:A a :B b :C c :PC 0 :program parsed-prog}}))

(defn operand [state]
  (get (:program state) (+ 1 (:PC state))))

(defn combo [state]
  (case (operand state)
    0 0
    1 1
    2 2
    3 3
    4 (state :A)
    5 (state :B)
    6 (state :C)
    7 (throw (Exception. "reserved"))))

(defn literal [state] (operand state))

(defn print-combo [operand]
  (case operand
    0 "0"
    1 "1"
    2 "2"
    3 "3"
    4 "a"
    5 "b"
    6 "b"
    7 (throw (Exception. "reserved"))))

(defn print-literal [operand]
  (str operand))

(defn print-instr [instr operand]
  (case instr
    :adv (format "a = a / (2^(%s))" (print-combo operand))
    :bxl (format "b = b xor %s" (print-literal operand))
    :bst (format "b = mod(%s, 8)" (print-combo operand))
    :jnz (format "if a != 0 goto %s" (print-literal operand))
    :bxc (format "b = b xor c")
    :out (format "output(mod(%s, 8))" (print-combo operand))
    :bdv (format "b = a / (2^(%s))" (print-combo operand))
    :cdv (format "c = a / (2^(%s))" (print-combo operand))))

(def instr
  {:adv (fn [state]
          (let [numer (state :A)
                denom (bit-shift-left 1 (combo state))
                result (quot numer denom)]
            {:A result}))
   :bxl (fn [state]
          {:B (bit-xor (state :B) (literal state))})
   :bst (fn [state]
          {:B (mod (combo state) 8)})
   :jnz (fn [state]
          (let [target (literal state)
                jump (not= 0 (state :A))]
            (if jump {:PC target} {})))
   :bxc (fn [state]
          {:B (bit-xor (state :B) (state :C))})
   :out (fn [state]
          {:output (mod (combo state) 8)})
   :bdv (fn [state]
          (let [numer (state :A)
                denom (bit-shift-left 1 (combo state))
                result (quot numer denom)]
            {:B result}))
   :cdv (fn [state]
          (let [numer (state :A)
                denom (bit-shift-left 1 (combo state))
                result (quot numer denom)]
            {:C result}))})

(defn from-opcode [opcode]
  (case opcode
    0 :adv
    1 :bxl
    2 :bst
    3 :jnz
    4 :bxc
    5 :out
    6 :bdv
    7 :cdv))

(defn run-at-pc [state]
  (let [opcode (get-in state [:program (state :PC)])
        result ((instr (from-opcode opcode)) state)
        merged (merge state {:PC (+ 2 (state :PC))} result)]
    merged))

(defn run-to-completion-inner [state visited-states]
  (if (visited-states state)
    (throw (Exception. "diverges"))
    (let [visited-states (conj visited-states state)
          state (run-at-pc state)
          next-pc (state :PC)
          output (:output state)
          state (dissoc state :output)]
      (if
       (nil? (get-in state [:program next-pc]))
        (if (nil? output) '() [output])
        (if (nil? output)
          (recur state visited-states)
          (cons output (lazy-seq (run-to-completion-inner state visited-states))))))))

(defn run-to-completion [state] (run-to-completion-inner state #{}))

(def part1 (->> (run-to-completion (:state parsed-input))
                (str/join ",")))

(printf "part1: %s%n" part1)

(defn a-when-0 [amod8]
  (*
   (bit-shift-left 1 (bit-xor 3 amod8))
   (bit-xor 6 amod8)))

(map a-when-0 (range 8))

(def part2 (* 5 (reduce * (repeat 15 (bigint 8)))))

;; (printf "part2: %s%n" part2)

(doseq [[opcode operand] (partition 2 (:program (:state parsed-input)))]
  (println (print-instr (from-opcode opcode) operand)))

(defn run-with-a [a] (run-to-completion (assoc (:state parsed-input) :A a)))

(defn exp8 [n] (reduce * 1 (repeat n 8)))

(defn range-poss [prev-a] (range (* 8 prev-a) (* 8 (+ 1 prev-a))))

(defn ends-with? [coll suffix]
  (= suffix (take-last (count suffix) coll)))

(def revprog (into [] (reverse (:program (:state parsed-input)))))

(run-with-a 1744494)

(defn output-when-a [a]
  (let [amod8 (mod a 8)]
    (mod
     (bit-xor
      6
      amod8
      (quot
       a
       (bit-shift-left 1 (bit-xor 3 amod8)))) 8)))

(defn next-a [a]
  (quot a 8))

(defn run-with-a [a]
  (if (= 0 a)
    nil
    (cons (output-when-a a) (run-with-a (next-a a)))))

(run-with-a 21539243)

(defn filter-possibilities [possibilities]
  (->> possibilities
       (filter (fn [a] (ends-with? (:program (:state parsed-input)) (run-with-a a))))))

;; (->> (iterate (fn [poss]
;;                 (println "iter")
;;                 (let [poss (into #{} (mapcat range-poss poss))
;;                       poss (filter-possibilities poss)] (take 10 poss))) #{6})
;;      (take 15)
;;      )

(def part2 (nth (->> (iterate (fn [poss]
                                (println poss)
                                (let [poss (into #{} (mapcat range-poss poss))
                                      poss (filter-possibilities poss)] poss)) #{6})) 15))

(println "actual part2" (apply min part2))

;;
;; (def part2 (->> (range)
;;                 (filter (fn [a] 
;;                           (if(= (:program (:state parsed-input)) (run-with-a a)) 
;;                             true
;;                             (do 
;;                               (println "ruled out" a)
;;                               false
;;                             ))))
;;                 (first)))
;;
;; (comment start point is 40987800)
;;
;; (printf "part2: %s%n" part2)
