(ns aoc.core
    (:require [clojure.string :as str]))

(defn parse-int [s] (Integer/parseInt s))
(defn read-lines [filename]
  (->> (slurp filename) str/split-lines))
