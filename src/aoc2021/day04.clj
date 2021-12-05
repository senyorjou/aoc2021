(ns aoc2021.day04
  (:require [clojure.string :as str]))

(def data (slurp "resources/day04.data"))


(def D
"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")


(defn ch->int [ch]
  (Integer/parseInt (str ch)))


(defn parse-board [board]
  (->> board
       (re-seq #"\d+")
       (mapv (fn [v] {:number (ch->int v) :hit false}))))


(defn parse [s]
  (let [[raw-numbers & raw-boards] (str/split s #"\n\n")
        numbers (->> raw-numbers
                     (re-seq #"\d+")
                     (mapv ch->int))
        boards (mapv parse-board raw-boards)]
    [numbers boards]))


(defn board-rows [board]
  (partition 5 board))


(defn board-columns [board]
  (map #(take-nth 5 (drop % board)) (range 5)))


(defn play-board [board drawn]
  (map (fn [row]
         (if (= (:number row) drawn)
           (update row :hit not)
           row))
       board))


(defn winning-lines [lines]
  (let [line-hits (map (fn [line]
                        (every? true? (map :hit line))) lines)]
  (some true? line-hits)))


(defn sum-unmarked [board]
  (apply + (map :number (remove :hit board))))


(defn winning-board [board]
  (let [rows (board-rows board)
        columns (board-columns board)]
    (if (true? (or (winning-lines rows)
                   (winning-lines columns)))
      board
      nil)))

(defn p1 [input]
  (let [parsed (parse input)
        numbers (first parsed)]
    (loop [index 0
           boards (map #(play-board % (nth numbers index)) (second parsed))]
      (let [winners (filter some? (map #(winning-board %) boards))]
        (if (seq winners)
          (let [winner-board (first winners)
                lucky-number (nth numbers index)
                unmarked (sum-unmarked winner-board)]
            (println "We have a winner")
            (* lucky-number unmarked))

          (recur (inc index)
                 (map #(play-board % (nth numbers (inc index))) boards)))))))

(defn split-reds [reds]
  (let [[losts wins] (split-with #(not (winning-board %)) reds)
        winner (first wins)]
    {:index (dec (dec (count losts))) ;; - initial - post play
     :unmarked (sum-unmarked winner)}))


(defn p2 [input]
  (let [[numbers boards] (parse input)
        reds (map #(reductions play-board % numbers) boards)
        splits (map split-reds reds)
        looser (last (sort-by :index splits))]
    (* (:index looser) (:unmarked looser))))
