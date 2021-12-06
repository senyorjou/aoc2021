(ns aoc2021.day05
  (:require [clojure.string :as str]))


(def data (slurp "resources/day05.data"))


(def D
"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn ch->int [ch]
  (Integer/parseInt (str ch)))

(defn parse-line [s]
  (->> (re-seq #"\d+" s)
       (mapv ch->int)))


(defn parse [input]
  (->> input
       str/split-lines
       (map parse-line)))


(defn hor-or-vert? [[x1 y1 x2 y2]]
  (or (= x1 x2)
      (= y1 y2)))


(defn line-coords [[x1 y1 x2 y2]]
  (let [max-x (max x1 x2)
        min-x (min x1 x2)
        max-y (max y1 y2)
        min-y (min y1 y2)]
    (for [x1->x2 (range min-x (inc max-x))
          y1->y2 (range min-y (inc max-y))]
       [x1->x2 y1->y2])))


(defn p1 [input]
  (->> input
       (filter hor-or-vert?)
       (mapcat line-coords)
       (frequencies)
       (vals)
       (filter #(> % 1))
       count))
