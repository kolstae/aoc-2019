(ns day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day24.txt")))

(defn parse-input [s]
  (into {}
        (mapcat (fn [y line] (map (fn [x c] [[x y] c]) (range) line))
                (range)
                (str/split-lines s))))

(defn draw-screen [tiles]
  (let [min-x (dec (apply min (map first (keys tiles))))]
    (->> tiles
         (sort-by (comp second first))
         (partition-by (comp second first))
         (map (fn [line] (->> (sort-by first line)
                              (map (fn [[[x] c]] [x (if (keyword? c) \@ c)]))
                              (cons [min-x \space])
                              (partition-all 2 1)
                              (mapcat (fn [[[x c] [x2]]]
                                        (cons c (when x2 (repeat (dec (- x2 x)) \space)))))
                              (apply str)))))))

(defn print-screen [tiles]
  (doseq [l (draw-screen tiles)] (println l)))

(def deltas [[0 -1] [1 0] [0 1] [-1 0]])

(defn tick [tiles p v]
  (let [cnt (count (filter #(= % \#) (map #(tiles (mapv + p %)) deltas)))]
    (case v
      \# (when (not= 1 cnt) \.)
      \. (when (<= 1 cnt 2) \#))))

(defn bio-div-rating [tiles]
  (reduce + 0 (map (fn [[x y]] (bit-shift-left 1 (+ x (* 5 y)))) (keys (filter #(= \# (val %)) tiles)))))

(defn part-1 [s]
  (let [tiles (parse-input s)
        rep (loop [tiles tiles seen #{}]
              (let [ts (reduce (fn [ts [p v]] (if-let [c (tick tiles p v)] (assoc ts p c) ts))
                               tiles tiles)]
                (if (contains? seen ts) ts (recur ts (conj seen ts)))))]
    (bio-div-rating rep)))

(comment
  (part-1 "....#\n#..#.\n#..##\n..#..\n#....")
  (part-1 input)                                            ;12531574
  )

(defn empty-grid [lvl]
  (for [x (range 5) y (range 5) :when (not= x y 2)]
    [[x y lvl] \.]))

(defn count-adjacent [tiles]
  (fn [pos deltas] (count (filter #(= % \#) (map #(tiles (mapv + pos %)) deltas)))))

(defn recur-tick [tiles p v]
  (let [adj-cnt-fn (count-adjacent tiles)
        cnt (adj-cnt-fn p [[0 -1 0] [1 0 0] [0 1 0] [-1 0 0]])
        [x y l] p
        e-cnt (+ cnt
                 (case x
                   0 (case y
                       0 (adj-cnt-fn [2 2 l] [[-1 0 -1] [0 -1 -1]])
                       4 (adj-cnt-fn [2 2 l] [[-1 0 -1] [0 1 -1]])
                       (adj-cnt-fn [2 2 l] [[-1 0 -1]]))
                   4 (case y
                       0 (adj-cnt-fn [2 2 l] [[1 0 -1] [0 -1 -1]])
                       4 (adj-cnt-fn [2 2 l] [[1 0 -1] [0 1 -1]])
                       (adj-cnt-fn [2 2 l] [[1 0 -1]]))
                   1 (case y
                       2 (adj-cnt-fn [2 2 l] [[-2 -2 1] [-2 -1 1] [-2 0 1] [-2 1 1] [-2 2 1]])
                       0 (adj-cnt-fn [2 2 l] [[0 -1 -1]])
                       4 (adj-cnt-fn [2 2 l] [[0 1 -1]])
                       0)
                   3 (case y
                       2 (adj-cnt-fn [2 2 l] [[2 -2 1] [2 -1 1] [2 0 1] [2 1 1] [2 2 1]])
                       0 (adj-cnt-fn [2 2 l] [[0 -1 -1]])
                       4 (adj-cnt-fn [2 2 l] [[0 1 -1]])
                       0)
                   2 (case y
                       1 (adj-cnt-fn [2 2 l] [[-2 -2 1] [-1 -2 1] [0 -2 1] [1 -2 1] [2 -2 1]])
                       3 (adj-cnt-fn [2 2 l] [[-2 2 1] [-1 2 1] [0 2 1] [1 2 1] [2 2 1]])
                       0 (adj-cnt-fn [2 2 l] [[0 -1 -1]])
                       4 (adj-cnt-fn [2 2 l] [[0 1 -1]])
                       0)
                   (case y
                     0 (adj-cnt-fn [2 2 l] [[0 -1 -1]])
                     4 (adj-cnt-fn [2 2 l] [[0 1 -1]])
                     0)))
        ;_ (when (neg? l) (prn p cnt e-cnt))
        ]
    (case v
      \# (when (not= 1 e-cnt) \.)
      \. (when (<= 1 e-cnt 2) \#))))

(defn part-2 [n s]
  (let [tiles (->> (dissoc (parse-input s) [2 2])
                   (map (fn [[k v]] [(conj k 0) v]))
                   (concat (empty-grid -1) (empty-grid 1))
                   (into {}))
        tick-fn (fn [tiles]
                  (let [ts (reduce (fn [ts [p v]]
                                     (if-let [c (recur-tick tiles p v)]
                                       (assoc ts p c)
                                       ts))
                                   tiles tiles)
                        non-e (distinct (map #(last (first %)) (filter #(= \# (second %)) ts)))
                        lvls (distinct (map #(last (first %)) ts))
                        ts (cond-> ts
                             (= (apply max non-e) (apply max lvls)) (into (empty-grid (inc (apply max lvls))))
                             (= (apply min non-e) (apply min lvls)) (into (empty-grid (dec (apply min lvls)))))]
                    ts))
        tiles-after-n (first (drop n (iterate tick-fn tiles)))]
    #_(doseq [[k v] (sort-by first (group-by (comp last first) tiles-after-n))]
      (println "level: " k)
      (print-screen v))
    (count (filter (comp #{\#} second) tiles-after-n))))

(comment
  (print-screen (empty-grid 1))
  (part-2 10 "....#\n#..#.\n#.?##\n..#..\n#....")
  (part-2 200 input)                                        ;2033
  )
