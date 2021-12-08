(ns aoc2021.day08
  (:require [clojure.string :as str]))


(def data (str/split-lines (slurp "resources/day08.data")))

(def D (str/split-lines
"be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))


(defn parse-line [line]
  (let [[input output] (str/split line #" \| " )]
       [(re-seq #"\w+" input)
        (re-seq #"\w+" output)]))

(defn parse-data [input]
  (map parse-line input))


(defn count-output [line]
  (let [input (second line)
        sizes (map count input)]
    (count (filter #{2 3 4 7} sizes))))

;; --- Part 1

(defn p1 [input]
  (apply + (map count-output (parse-data input))))

;; --- Part 2

(defn find-1 [input]
  (= (count input) 2))

(defn find-4 [input]
  (= (count input) 4))

(defn find-7 [input]
  (= (count input) 3))

(defn find-8 [input]
  (= (count input) 7))

(defn find-6 [input match]
  ;; 6 is len 6 and NOT a subset of 1
  ;; (= (count input) 6))
  (and (= (count input) 6)
       (not (clojure.set/subset? match (set input)))))

(defn find-9 [input match]
  ;; 9 is len 6 and a subset of 4
  ;; (= (count input) 6))
  (and (= (count input) 6)
       (clojure.set/subset? match (set input))))


(defn get-set [n line]
  (let [matches (filter #(= n (count %)) line)]
    (first (map set matches))))

(defn get-sub [n line]
  (filter #(= n (count %)) line))


(defn get-nums [line]
  (let [set-1 (get-set 2 line)
        set-4 (get-set 4 line)
        set-7 (get-set 3 line)
        set-8 (get-set 7 line)
        sub-069 (get-sub 6 line)
        leds-6 (filter #(find-6 % set-1) sub-069)
        leds-9 (filter #(find-9 % set-4) sub-069)]

    [set-1 set-4 set-7 set-8 leds-6 leds-9 sub-069]))

(comment

  (apply + (map count-output (parse-data D)))

  ;;
  (def bits-groups {
                    2 {1 "0010010"}
                    3 {7 "1010010"}
                    4 {4 "0111010"}
                    5 {2 "1011101" 3 "1011011" 5 "1101011"}
                    6 {0 "1110111" 6 "1101111" 9 "1111011"}
                    7 {8 "1111111"}})

  (def bits {
        0 "1110111"
        1 "0010010"
        2 "1011101"
        3 "1011011"
        4 "0111010"
        5 "1101011"
        6 "1101111"
        7 "1010010"
        8 "1111111"
        9 "1111011"})
  )
