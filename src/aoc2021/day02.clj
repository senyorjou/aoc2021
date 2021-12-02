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

;; morning, morning
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


(defn p1-old []
  (->> data
       (map parse-line)
       (process-prod)))

;; night nigth

(defn steps [state [action value]]
  (case action
    "forward" (update state "horizontal" + value)
    "up"      (update state "depth"      - value)
    "down"    (update state "depth"      + value)))

(defn aimed-steps [state [direction value]]
  (let [current-aim (get state "aim")]
  (case direction
    "forward" (-> state
                 (update "horizontal" + value)
                 (update "depth"      + (* value current-aim)))
    "up"      (update state "aim" - value)
    "down"    (update state "aim" + value))))


(defn multiply [vals]
  (* (first vals) (second vals)))


(defn solve [fn initial]
  (->> data
       (map parse-line)
       (reduce fn initial)
       vals
       multiply))

(defn p1 []
  (solve steps {"horizontal" 0 "depth" 0}))

(defn p2 []
  (solve aimed-steps {"horizontal" 0 "depth" 0 "aim" 0}))
