(ns aoc2021.day02
  (:require [clojure.string :as str]))

(def data (str/split-lines (slurp "resources/day02.data")))

;; (def data
;;   ["forward 5"
;;    "down 5"
;;    "forward 8"
;;    "up 3"
;;    "down 8"
;;    "forward 2"])


(defn parse-line [line]
  (let [[action value] (str/split line #" ")]
    [action (Integer/parseInt value)]))

(defn fwd? [[action _]]
  (= action "forward"))

(defn up? [[action _]]
  (= action "up"))

(defn down? [[action _]]
  (= action "down"))

(defn sum-second [lines]
  (apply + (map second lines)))

(defn process-prod [lines]
  (let [forwards (sum-second (filter fwd? lines))
        ups (sum-second (filter up? lines))
        downs (sum-second (filter down? lines))]
    (* forwards (- downs ups))))

(defn p1 []
  (->> data
       (map parse-line)
       (process-prod)))
