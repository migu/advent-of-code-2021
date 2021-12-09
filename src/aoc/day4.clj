(ns aoc.day4
  (:require [aoc.core :refer :all]
            [clojure.string :as str]
            [aocd.core :as data]
            [clojure.zip :as zip]))

(defn test-data [] (slurp "resources/4/test-input"))

(defn parse-ints [line]
  (let [line (str/trim line)
        create-cell (fn [s] [(parse-int s) false])]
    (mapv create-cell (str/split line #"\s+"))))

(defn create-matrix [lines]
  (mapv parse-ints lines))

(defn extract-data [input]
  (let [lines (str/split-lines input)
        numbers (mapv parse-int (-> lines first (str/split #",")))
        fields (mapv create-matrix (partition 5 6 (nnext lines)))]
    {:numbers numbers :fields fields}))

(defn line-full [line]
  (every? true? (map #(nth % 1) line)))
(defn transpose [m]
  (apply mapv vector m))
(defn matrix-wins [matrix]
  (or (some? (some line-full matrix))
      (some? (some line-full (transpose matrix)))))
(defn sum-of-unmarked-numbers [m]
  (->> m
       (reduce into)
       (filterv #(false? (nth % 1)))
       (mapv #(nth % 0))
       (reduce +)))

(defn zip-map [f loc]
  (loop [z loc]
    (if (zip/end? z)
      (zip/root z)
      (recur (zip/next (zip/edit z f z))))))
(defn mark-number [m number]
  (zip-map (fn [n nx] (if (and (vector? n) (= number (nth n 0))) (assoc n 1 true) n)) (zip/vector-zip m)))

(defn find-winners [input]
  (let [{:keys [numbers fields]} (extract-data input)]
    (loop [numbers numbers
           fields fields
           winners []
           winning-numbers []]
      (let [number (first numbers)
            updated-fields (mapv #(mark-number % number) fields)
            new-winners (filterv matrix-wins updated-fields)]
        (if (nil? number)
          {:winning-numbers winning-numbers :winners (mapv first winners)}
          (if (seq new-winners)
            (recur (rest numbers)
                   (filterv (fn [f] (not (some #{f} new-winners))) updated-fields)
                   (conj winners new-winners)
                   (conj winning-numbers number))
            (recur (rest numbers)
                   updated-fields
                   winners
                   winning-numbers)))))))

(defn score-of-winner [input select]
  (let [{:keys [winning-numbers winners]} (find-winners input)
        number (select winning-numbers)
        winner (select winners)]
    (* number (sum-of-unmarked-numbers winner))))
(defn score-of-first-winner [input]
  (score-of-winner input first))
(defn score-of-last-winner [input]
  (score-of-winner input last))

(println (score-of-first-winner (test-data)))
(println (score-of-first-winner (data/input 2021 4)))

(println (score-of-last-winner (test-data)))
(println (score-of-last-winner (data/input 2021 4)))
