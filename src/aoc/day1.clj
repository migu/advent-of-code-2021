(ns aoc
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def filename "input1")
(defn parse-int [s] (Integer/parseInt s))
(defn read-numbers-from-file []
  (->> (slurp filename)
       str/split-lines
       (map parse-int)))

(defn update-windows [windows numbers window-size]
  (let [sum (reduce + (take window-size numbers))
        windows (conj windows sum)]
    (if (> (count windows) 2)
      (subvec windows 1)
      windows)))

(defn evaluate-increase [windows sum]
  (if (and (>= (count windows) 2) (< (nth windows 0) (nth windows 1)))
    (inc sum)
    sum))

(defn count-increases [numbers window-size]
  (loop [windows []
         numbers numbers
         sum 0]
    (if (< (count numbers) window-size)
      (evaluate-increase windows sum)
      (recur (update-windows windows numbers window-size) (next numbers) (evaluate-increase windows sum)))))

(def numbers (read-numbers-from-file))
(println (count-increases numbers 1))
(println (count-increases numbers 3))
