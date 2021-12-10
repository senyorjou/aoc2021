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

(def points {
             \) 3
             \] 57
             \} 1197
             \> 25137})

(defn process-line
  ([sequence]
   (process-line sequence []))
  ([sequence stack]
    (let [token (first sequence)]
      (if (nil? token) ;; last
        nil
        (if (pairs token) ;; is opening, push and continue
          (recur (rest sequence)
                 (conj stack token))
          (if (= token (pairs (last stack))) ;; matches ">" (get pairs "<")
            (recur (rest sequence)
                   (pop stack))
            ;; none of the above... this is the guy
            token))))))


(defn p1 [input]
  (->> input
       (map process-line)
       (filter some?)
       (map points)
       (reduce +)))



(comment
  (def line [\[ \( \{ \( \< \( \( \) \) \[ \] \> \[ \[ \{ \[ \] \{ \< \( \) \< \> \]])
  (get pairs (first line))

  (balance line)
  (def brackets {\[ \] \( \) \{ \} \< \>})
  )
