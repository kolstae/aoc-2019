(ns day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-int [s]
  (Integer/parseInt s))

(def input (take 2 (str/split-lines (slurp (io/resource "day3.txt")))))

(defn parse-wire [s]
  (->> (str/split s #",")
       (map (fn [op]
              (conj (case (first op) \R [+ 0] \L [- 0] \U [+ 1] \D [- 1])
                    (parse-int (.substring op 1)))))
       (reduce (fn [ps [op k len]] (conj ps (-> (last ps) (update k op len) (update 2 + len))))
               [[0 0 0]])))

(defn ps->lines [ps]
  (map vector ps (next ps)))

(defn manhattan-dist [^long x ^long y]
  (+ (Math/abs x) (Math/abs y)))

(defn diff [^long x ^long y]
  (Math/abs (- x y)))

(defn crossing [[[x11 y11 l1] [x12 y12]] [[x21 y21 l2] [x22 y22]]]
  (cond
    (and (= y11 y12) (= x21 x22)
         (< (min x11 x12) x21 (max x11 x12))
         (< (min y21 y22) y11 (max y21 y22))) [(manhattan-dist x21 y11) x21 y11 (+ l1 l2 (diff x11 x21) (diff y11 y21))]
    (and (= x11 x12) (= y21 y22)
         (< (min x21 x22) x11 (max x21 x22))
         (< (min y11 y12) y21 (max y11 y12))) [(manhattan-dist x11 y21) x11 y21 (+ l1 l2 (diff x11 x21) (diff y11 y21))]))

(defn part-1 [w1 w2]
  (->> (for [l1 (ps->lines (parse-wire w1)) l2 (ps->lines (parse-wire w2))]
         (crossing l1 l2))
       (keep first)
       (apply min)))

(comment
  (ps->lines (parse-wire "R8,U5,L5,D3"))
  (part-1 "R8,U5,L5,D3" "U7,R6,D4,L4")
  (part-1 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83")
  (part-1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
  (apply part-1 input)                                      ;651
  )

(defn part-2 [w1 w2]
  (->> (for [l1 (ps->lines (parse-wire w1)) l2 (ps->lines (parse-wire w2))]
         (crossing l1 l2))
       (keep last)
       (apply min)))

(comment
  (ps->lines (parse-wire "R8,U5,L5,D3"))
  (part-2 "R8,U5,L5,D3" "U7,R6,D4,L4")
  (part-2 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83")
  (part-2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
  (apply part-2 input)                                      ;7534
  )
