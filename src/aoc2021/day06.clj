(ns aoc2021.day06
  (:require [clojure.string :as str]))


(def data (slurp "resources/day06.data"))

(def D "3,4,3,1,2")

(def initial-population {:0 0 :1 0 :2 0 :3 0 :4 0 :5 0 :6 0 :7 0 :8 0})

(defn parse-keys [s]
  (->> (re-seq #"\d+" s)
       (map keyword)))

(defn generation [state _]
  {:0 (:1 state)
   :1 (:2 state)
   :2 (:3 state)
   :3 (:4 state)
   :4 (:5 state)
   :5 (:6 state)
   :6 (+ (:7 state) (:0 state))
   :7 (:8 state)
   :8 (:0 state)})

(defn solve [input days]
  (let [initial
        (->> input
             parse-keys
             frequencies
             (merge initial-population))]
    (apply + (vals (reduce generation initial (range days))))))

(defn p1 [] (solve data 80))
(defn p2 [] (solve data 256))
