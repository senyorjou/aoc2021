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


(defn p1 [input]
  (apply + (map count-output (parse-data input))))


(comment

  (apply + (map count-output (parse-data D)))
  (->> (parse-data D)
       (map second)
       (filter )
       (map count))
)
