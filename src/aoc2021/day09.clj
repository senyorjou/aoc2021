
(ns aoc2021.day09
  (:require [clojure.string :as str]))


(def data (slurp "resources/day09.data"))

(def D "2199943210
3987894921
9856789892
8767896789
9899965678")

(defn ch->int [ch]
  (Integer/parseInt (str ch)))

(defn parse-data [text]
  (let [rows (str/split-lines text)
        columns (count rows)
        edge-x (count (first rows))
        board  (into {}
                    (for [y (range columns)
                          :let [line (nth rows y)]
                          x (range (count line))]
                      [[x y] (ch->int (nth line x) )]))]
    {:board board
     :edge-x edge-x
     :edge-y columns}))



(defn edge-filter [edge-x edge-y [x y]]
  (and (>= x 0)
       (>= y 0)
       (< x edge-x)
       (< y edge-y)))

(defn adjacents [edge-x edge-y [x y]]
  (let [all-points [[(inc x) y]
                    [(dec x) y]
                    [x (inc y)]
                    [x (dec y)]]]
    (filter #(edge-filter edge-x edge-y %) all-points)))

(defn val-is-lower [[vals val]]
  (every? true? (map #(< val %) vals)))


(defn p1 [data]
  (let [input (parse-data data)
        {:keys [board edge-x edge-y]} input
        vals-for-val (map (fn [[coords v]]
                            [(map board (adjacents edge-x edge-y coords)) v]) board)
        lower-vals (filter val-is-lower vals-for-val)]
    (reduce + (map #(inc (second %)) lower-vals))))
