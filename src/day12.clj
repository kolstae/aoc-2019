(ns day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day12.txt")))

(defn parse-int [s]
  (Long/parseLong s))

(defn parse-input [s]
  (map #(->> (str/split % #"[<>=,\sxyz]+")
             (remove empty?)
             (mapv parse-int))
       (str/split-lines s)))

(defn a-velocity [a b]
  (if (> a b) -1 (if (= a b) 0 1)))

(defn veloity [{p :pos v :vel :as x} ps]
  (let [v (reduce (fn [v {p2 :pos}] (map + v (map a-velocity p p2)))
                  v
                  (remove #{x} ps))]
    {:pos (map + v p) :vel v}))

(defn abs [^long n] (Math/abs n))

(defn abs+ [^long a ^long b] (+ (Math/abs a) (Math/abs b)))

(defn energy [ps]
  (reduce (fn [e p] (+ e (* (reduce abs+ (:pos p)) (reduce abs+ (:vel p))))) 0 ps))

(defn part-1 [n s]
  (let [ps (map #(hash-map :pos % :vel [0 0 0]) (parse-input s))]
    (energy (first (drop n (iterate (fn [ps] (map #(veloity % ps) ps)) ps))))))

(comment
  (part-1 10 "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>")
  (part-1 100 "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>")
  (part-1 1000 input)                                       ;8362
  )

(defn freq [ps]
  (loop [ps ps seen #{}]
    (if (contains? seen ps)
      (count seen)
      (recur (map #(veloity % ps) ps) (conj seen ps)))))

(defn gcd [a b]
  (if (zero? a) b (recur (mod b a) a)))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn part-2 [s]
  (let [[fx fy fz] (map (fn [a] (freq (map #(hash-map :pos [(nth % a)] :vel [0]) (parse-input s)))) (range 3))]
    (lcm (lcm fx fy) fz)))

(comment
  (part-2 "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>")
  (part-2 "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>")
  (part-2 input)                                            ;478373365921244
  )
