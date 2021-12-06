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

(defn max-min [[x1 y1 x2 y2]]
  (let [max-x (max x1 x2)
        min-x (min x1 x2)
        max-y (max y1 y2)
        min-y (min y1 y2)]
  [min-x min-y max-x max-y]))


(defn line-coords [line]
  (let [[x1 y1 x2 y2] (max-min line)]
    (for [xss (range x1 (inc x2))
          yss (range y1 (inc y2))]
       [xss yss])))


(defn p1 [input]
  (->> input
       (filter hor-or-vert?)
       (mapcat line-coords)
       (frequencies)
       (vals)
       (filter #(> % 1))
       count))


(defn diagonal-range [x1 x2]
  (if (<= x1 x2)
    (range x1 (inc x2))
    (range x1 (dec x2) -1)))


(defn diagonal-coords [[x1 y1 x2 y2]]
  (map vector (diagonal-range x1 x2) (diagonal-range y1 y2)))


(defn line-dispatcher [line]
  (if (hor-or-vert? line)
    (line-coords line)
    (diagonal-coords line)))


(defn p2 [input]
  (->> input
       (mapcat line-dispatcher)
       (frequencies)
       (vals)
       (filter #(> % 1))
       count))
