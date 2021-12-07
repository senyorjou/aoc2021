(ns aoc2021.day07
  (:require [clojure.string :as str]))


(def data (slurp "resources/day07.data"))

(def D "16,1,2,0,4,2,7,1,2,14")


(defn ch->int [ch]
  (Integer/parseInt (str ch)))


(defn parse-data [s]
  (->> (re-seq #"\d+" s)
       (map ch->int)))


(defn median [ns]
  ;; https://rosettacode.org/wiki/Averages/Median#Clojure
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)] ;; LOL
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn sum-to-n [n]
  ;; n * (n + 1) / 2
  (* n (/ (inc n) 2)))


(defn fuel-needed [to from]
  (Math/abs (- from to)))

(defn calc-fuel [input]
  "Creates a list of calculated distances agaist median"
  (let [destination (median input)
        fuel-fn (partial fuel-needed destination)]
    (map fuel-fn input)))

(defn pfuel-needed [start end pos]
  "Creates a list of fuel needed from first pos last"
  (let [dist-to-start (Math/abs (- start pos))
        dist-to-end (Math/abs (- end pos))]
    [(sum-to-n dist-to-start) (sum-to-n dist-to-end)]))


(defn p1 []
  (reduce + (calc-fuel (parse-data data))))


(comment
  (median (parse-data D))
  (sort (parse-data D))
  (calc-fuel (parse-data D))
  (take 10 (parse-data data))
  (median [1 1 1 1 6 6 6 6])


  (calc-fuel (parse-data data))
  (println (p1))

  (calc-pfuel 1000 1)

)



(comment



;; Move from 16 to 5: 66 fuel
;; Move from 1 to 5: 10 fuel
;; Move from 2 to 5: 6 fuel
;; Move from 0 to 5: 15 fuel
;; Move from 4 to 5: 1 fuel
;; Move from 2 to 5: 6 fuel
;; Move from 7 to 5: 3 fuel
;; Move from 1 to 5: 10 fuel
;; Move from 2 to 5: 6 fuel
;; Move from 14 to 5: 45 fuel
)
