(ns aoc2021.day10
  (:require [clojure.string :as str]))


(def data (slurp "resources/day10.data"))

(def D "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")


(defn parse-data [input]
  (->> input
       (str/split-lines)
       (map seq)))

(def pairs {
            \[ \]
            \{ \}
            \( \)
            \< \>})

(def points-p1 {
                \) 3
                \] 57
                \} 1197
                \> 25137})

(def points-p2 {
                \) 1
                \] 2
                \} 3
                \> 4})


(defn process-line
  ([sequence]
   (process-line sequence []))
  ([sequence stack]
    (let [token (first sequence)]
      (if (nil? token) ;; last
        [nil stack]
        (if (pairs token) ;; is opening, push and continue
          (recur (rest sequence)
                 (conj stack token))
          (if (= token (pairs (last stack))) ;; matches ">" (get pairs "<")
            (recur (rest sequence)
                   (pop stack))
            ;; none of the above... this is the guy
            [token nil]))))))


(defn rating [acc v]
  (+ (* acc 5) v))

(defn get-p2-points [tokens]
  (let [closing (reverse (map pairs tokens))]
    (reduce rating (map points-p2 closing))))

(defn p1 [input]
  (->> input
       (map process-line)
       (map first)
       (filter some?)
       (map points-p1)
       (reduce +)))

(defn p2 [input]
  (let [values (->> input
                    (map process-line)
                    (map second)
                    (filter some?)
                    (map get-p2-points))
        middle (quot (count values) 2)]
    (first (drop middle (sort values)))))
