(ns day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day22.txt")))

(defn parse-int [s] (Long/parseLong s))

(defn parse-input [s]
  (map #(cond
          (str/starts-with? % "cut") [:cut (parse-int (subs % 4))]
          (str/starts-with? % "deal with increment") [:deal-inc (parse-int (subs % 20))]
          :else [:reverse])
       (str/split-lines s)))

(defn cut [d n]
  (let [idx (if (neg? n) (+ (count d) n) n)]
    (apply concat (reverse (split-at idx d)))))

(defn deal-inc [d n]
  (let [cnt (count d)]
    (into [] (map second) (sort-by first (map vector (iterate #(mod (+ n %) cnt) 0) d)))))

(defn part-1 [n s]
  (reduce (fn [d [op n]] (case op
                           :reverse (reverse d)
                           :deal-inc (deal-inc d n)
                           :cut (cut d n)))
          (range n)
          (parse-input s)))

(comment
  (deal-inc (range 10) 3)
  (deal-inc '(0 3 6 9 2 5 8 1 4 7) 9)
  (= (cut (range 10) -4) [6 7 8 9 0 1 2 3 4 5])
  (part-1 10 "deal with increment 7\ndeal into new stack\ndeal into new stack\n")
  (part-1 10 "deal with increment 7\ndeal with increment 9\ncut -2\n")
  (part-1 10 "deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\ndeal with increment 9\ndeal with increment 3\ncut -1")
  (ffirst (drop-while #(not= 2019 (second %)) (map-indexed vector (part-1 10007 input)))) ;4649
  )
