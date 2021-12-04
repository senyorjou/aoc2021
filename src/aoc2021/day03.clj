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
    (* mosts leasts))



;; (defn calc-value [churro]
;;   (let [churrito (apply str churro)
;;         freqs (frequencies churrito)
;;         zeroes (get freqs \0)
;;         ones (get freqs \1)]
;;     (if (> zeroes ones) 0 1)))

;; (defn columns [data]
;;   (let [churro (str/join data))
;;     (loop [col 1
;;            churrito (take-nth 5 churro)
;;            str-churro (apply str churrito)
;;            col-value (calc-value churrito) ]
;;       (if (> col 5))
;;       )
;;         col (take-nth 5 churro)
;;         str-col (apply str col)]
;;     str-col))
