(ns aoc.day2
  (:require [aoc.core :refer :all]))

(def filename "resources/2/input")
(def pattern #"(\w+) (\d+)$")
(def initial-state {:pos 0 :depth 0 :aim 0})

(defn parse-command [line]
  (when-let [m (re-matches pattern line)]
    {:command (second m) :value (parse-int (second (rest m)))}))

(defn read-input [filename]
  (->> (read-lines filename) (map parse-command)))

(defn cruise [update-fn actions]
  (reduce update-fn initial-state actions))

(defn update-state-1 [state action]
  (let [command (:command action)
        value (:value action)]
    (condp = command
      "forward" (update state :pos (partial + value))
      "up" (update state :depth #(- % value))
      "down" (update state :depth (partial + value))
      state)))

(defn update-state-2 [state action]
  (let [command (:command action)
        value (:value action)]
    (condp = command
      "forward" (assoc state
                       :pos (+ (:pos state) value)
                       :depth (+ (:depth state) (* (:aim state) value)))
      "up" (update state :aim #(- % value))
      "down" (update state :aim #(+ % value))
      state)))

(defn print-result [update-fn]
  (let [actions (read-input filename)
        state (cruise update-fn actions)]
    (println (* (:pos state) (:depth state)))))

(print-result update-state-1)
(print-result update-state-2)
