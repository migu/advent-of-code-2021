(ns aoc.day3
  (:require [aoc.core :refer :all]
            [clojure.string :as str]))

(def lines (read-lines "resources/3/input"))

(defn chars-to-int [cs]
  (mapv #(Character/digit % 10) cs))
(defn calc-sums [lines]
  (let [lines (map chars-to-int lines)
        len (count (first lines))
        start (->> 0 repeat (take len))
        sums (reduce #(map + %1 %2) start lines)]
    (vec sums)))
(defn calc-most-common-bits [sums count]
  (mapv #(if (>= % (/ count 2)) 1 0) sums))
(defn invert [xs]
  (mapv #(if (= 0 %) 1 0) xs))
(defn decode-binary [bits]
  (reduce + (map-indexed #(* %2 (int (Math/pow 2 %1))) (reverse bits))))

(defn calc-power-consumption [lines]
  (let [count (count lines)
        sums (calc-sums lines)
        mcbs (calc-most-common-bits sums count)
        gamma-rate (decode-binary mcbs)
        epsilon-rate (decode-binary (invert mcbs))]
    (* gamma-rate epsilon-rate)))

(println (calc-power-consumption lines))

(defn calc-rating [lines test]
  (let [lines (vec (map chars-to-int lines))
        len (count (first lines))
        select-lines (fn [lines i]
                       (let [cnt (count lines)
                             sum (reduce + (map #(nth % i) lines))]
                         (filter #(= (nth % i) (if (test cnt sum) 1 0)) lines))
                       )]
    (loop [lines lines
           i 0]
      (if (= 1 (count lines))
        (decode-binary (first lines))
        (recur (select-lines lines i) (inc i))))))

(defn calc-oxygen-gen-rating [lines]
  (calc-rating lines (fn [cnt sum] (>= sum (- cnt sum)))))
(defn calc-o2-scrubber-rating [lines]
    (calc-rating lines (fn [cnt sum] (< sum (- cnt sum)))))
(defn calc-life-support-rating [lines]
  (* (calc-oxygen-gen-rating lines) (calc-o2-scrubber-rating lines)))

(println (calc-life-support-rating lines))
