(ns aoc2021.day07
  (:require [clojure.string :as str]))


(def data (slurp "resources/day07.data"))

(def D "16,1,2,0,4,2,7,1,2,14")


(defn ch->int [ch]
  (Integer/parseInt (str ch)))


(defn parse-data [s]
  (->> (re-seq #"\d+" s)
       (map ch->int)))

;; PART 1
(defn median [ns]
  ;; https://rosettacode.org/wiki/Averages/Median#Clojure
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)] ;; LOL
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn sum-to-n [n]
  ";; n * (n + 1) / 2"
  (let [m (Math/abs n)]
    (* m (/ (inc m) 2))))


(defn fuel-needed [to from]
  (Math/abs (- from to)))

(defn calc-fuel [input]
  "Creates a list of calculated distances agaist median"
  (let [destination (median input)
        fuel-fn (partial fuel-needed destination)]
    (map fuel-fn input)))


(defn p1 [input]
  (reduce + (calc-fuel (parse-data input))))


;; PART 2

(defn consume-vector [input]
  "Global vector of consumptions"
  (let [edge (apply max input)]
    (map sum-to-n (range (inc (* -1 edge)) edge))))

(defn cv-for-pos [cv size pos]
  (into [] (take size (drop (- size pos) cv))))

(defn p2 [input]
  (let [parsed-data (parse-data input)
        global-vector (consume-vector parsed-data)
        data-len (apply max parsed-data)
        consume-vectors-for-pos (map #(cv-for-pos global-vector data-len %) parsed-data)]
    (->> consume-vectors-for-pos
         (apply map vector)  ;; transpose
         (map #(reduce + %)) ;; sum
         (apply min))))
