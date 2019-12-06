(ns day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day6.txt")))

(defn parse-input [s]
  (into {}
        (comp (map #(str/split % #"\)"))
              (map (fn [[v k]] [k v])))
        (str/split-lines s)))

(defn orbits-from [orbits start]
  (->> start
       (iterate #(orbits %))
       next
       (take-while identity)))

(defn part-1 [s]
  (let [orbits (parse-input s)]
    (->> (keys orbits)
         (map #(orbits-from orbits %))
         (map count)
         (reduce +))))

(comment
  (part-1 "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")
  (part-1 input)                                            ;254447
  )

(defn part-2 [s]
  (let [orbits (parse-input s)
        common-p (first (for [p1 (orbits-from orbits "YOU")
                              p2 (orbits-from orbits "SAN")
                              :when (= p1 p2)]
                          p1))]
    (+ (count (take-while (complement #{common-p}) (orbits-from orbits "YOU")))
       (count (take-while (complement #{common-p}) (orbits-from orbits "SAN"))))))

(comment
  (part-2 "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN")
  (part-2 input)                                            ;445
  )
