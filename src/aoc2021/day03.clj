(ns aoc2021.day03
  (:require [clojure.string :as str]))

(def data (str/split-lines (slurp "resources/day03.data")))

(def D
  ["00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"])

(def E "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

;; Part 1 get the matrix and TRANSPOSE-IT
;; https://stackoverflow.com/a/40154124
;;
;;Then apply calculations
;;


(defn ch->int [ch]
  (Integer/parseInt (str ch)))


(defn bitseq->int [seq]
  (Integer/parseInt (apply str seq) 2))


(defn parse-line [line]
  (map ch->int line))


(defn transpose [lines]
  (apply map vector lines))


(defn bit-pairs [line]
  (->> line
       transpose
       (mapv frequencies)))


(defn significant [line]
  (let [pairs (frequencies line)
        zeroes (get pairs 0)
        ones (get pairs 1)]
    (if (> zeroes ones)
      {:most 0 :least 1}
      {:most 1 :least 0})))


(defn p1 []
  (let [sigs (->> data
                  (map parse-line)
                  transpose
                  (map significant))
        mosts (bitseq->int (map :most sigs))
        leasts (bitseq->int (map :least sigs))]
    (* mosts leasts)))


(defn significant-bit [pairs]
  (let [zeroes (get pairs 0)
        ones (get pairs 1)]
    (if (> zeroes ones)
      {:most 0 :least 1}
      {:most 1 :least 0})))


(defn find-last-bucket [input most-or-least]
  (loop [numbers input
         idx 0]
    (let [bit    (most-or-least (-> numbers bit-pairs (nth idx) significant-bit))
          bucket (into [] (filter #(= bit (nth % idx))) numbers)]
      (if (= 1 (count bucket))
        (first bucket)
        (recur bucket (inc idx))))))

(defn p2 []
  (let [line-data (map parse-line data)
        O2 (bitseq->int (find-last-bucket line-data :most))
        CO2 (bitseq->int (find-last-bucket line-data :least))]
    (* O2 CO2)))
