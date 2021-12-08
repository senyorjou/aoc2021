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

(defn find-6 [input match]
  ;; 6 is len 6 and NOT a subset of 1
  (and (= (count input) 6)
       (not (clojure.set/subset? match (set input)))))

(defn find-9 [input match]
  ;; 9 is len 6 and a subset of 4
  (and (= (count input) 6)
       (clojure.set/subset? match (set input))))

(defn find-3 [input match]
  ;; 3 is len 5 and a subset of 1
  (and (= (count input) 5)
       (clojure.set/subset? match (set input))))

(defn find-5 [input match]
  ;; 5 is len 5 and a subset of 6
  (and (= (count input) 5)
       (clojure.set/subset? (set input) match)))


(defn get-set [n line]
  (let [matches (filter #(= n (count %)) line)]
    (first (map set matches))))

(defn get-sub [n line]
  (set (filter #(= n (count %)) line)))

(defn decode-entry [line]
  (let [set-1 (get-set 2 line)
        set-4 (get-set 4 line)
        set-7 (get-set 3 line)
        set-8 (get-set 7 line)
        sub-069 (get-sub 6 line)
        leds-6 (first (filter #(find-6 % set-1) sub-069))
        leds-9 (first (filter #(find-9 % set-4) sub-069))
        leds-0 (first (disj sub-069 leds-6 leds-9))
        sub-235 (get-sub 5 line)
        leds-3 (first (filter #(find-3 % set-1) sub-235))
        leds-5 (first (filter #(find-5 % (set leds-6)) sub-235))
        leds-2 (first (disj sub-235 leds-3 leds-5))]

    {set-1 1
     set-4 4
     set-7 7
     set-8 8
     (set leds-6) 6
     (set leds-9) 9
     (set leds-0) 0
     (set leds-3) 3
     (set leds-5) 5
     (set leds-2) 2}))

(defn match-dict [input output]
  (Integer/parseInt (apply str (map #(get input (set %)) output))))

(defn count-decoded [[input output]]
  (match-dict (decode-entry input) output))

(defn p2 [input]
  (reduce + (map #(count-decoded %) (parse-data input))))
