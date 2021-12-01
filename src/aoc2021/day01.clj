(ns aoc2021.day01
  (:require [clojure.string :as str]))


(def data (map #(Integer/parseInt %) (str/split-lines (slurp "resources/day01.data"))))

(def D [199 200 208 210 200 207 240 269 260 263])

(defn p1
  ([] (p1 data))
  ([input]
  (count (filter #(> (second %) (first %)) (partition 2 1 input)))))


(defn p2 []
  (let [threes (->> data
                    (partition 3 1)
                    (map #(reduce + %)))]
  (p1 threes)))
