(ns day17
  (:require [clojure.java.io :as io]
            [intcode]))

(def input (slurp (io/resource "day17.txt")))

(defn draw-screen [tiles]
  (let [min-x (dec (apply min (map first (keys tiles))))]
    (->> tiles
         (sort-by (comp second first))
         (partition-by (comp second first))
         (map (fn [line] (->> (sort-by first line)
                              (map (fn [[[x] c]] [x c]))
                              (cons [min-x \space])
                              (partition-all 2 1)
                              (mapcat (fn [[[x c] [x2]]]
                                        (cons c (when x2 (repeat (dec (- x2 x)) \space)))))
                              (apply str)))))))

(defn exec [memory]
  (let [initial-state {:memory memory :i-ptr 0 :input [] :relbase 0}]
    (loop [state initial-state]
      (if (int? (get state :i-ptr))
        (recur (intcode/do-next-opcode state))
        state))))

(defn intersection? [tiles pos]
  (every? (comp #{\#} tiles) (map #(mapv + pos %) [[0 -1] [1 0] [0 1] [-1 0]])))

(defn find-intersections [tiles]
  (filter #(intersection? tiles %) (keys tiles)))

(defn part-1 [s]
  (let [cs (:output (exec (intcode/parse-input s)))
        tiles (->> (map char cs)
                   (reduce (fn [[tiles [x y]] c] [(cond-> tiles (not (#{\newline \.} c)) (assoc [x y] c))
                                                  (if (= \newline c) [0 (inc y)] [(inc x) y])])
                           [{} [0 0]])
                   first)
        _ (prn (doseq [l (draw-screen tiles)] (println l)))]
    (->> tiles
         find-intersections
         (map #(apply * %))
         (reduce +))))

(comment
  (part-1 input)                                            ;4220
  )

(def dir->delta {:up [0 -1] :right [1 0] :down [0 1] :left [-1 0]})

(defn make-turn [tiles pos dir]
  (let [dirs (if (#{:up :down} dir) [:left :right] [:up :down])
        n-dir (first (filter #(tiles (mapv + (dir->delta %) pos)) dirs))]
    (when n-dir
      [n-dir (if (= n-dir (second (drop-while #(not= dir %) (cycle [:up :right :down :left]))))
               :right
               :left)])))

(defn strait-path [tiles pos dir]
  (let [delta (dir->delta dir)]
    (take-while tiles (drop 1 (iterate #(mapv + % delta) pos)))))

(defn find-path [tiles dir-turn start-pos]
  (loop [dir-turn dir-turn pos start-pos path []]
    (if dir-turn
      (let [[dir turn] dir-turn
            n-path (strait-path tiles pos dir)
            pos (last n-path)]
        (recur (make-turn tiles pos dir) pos (into (conj path turn (count n-path)))))
      path)))

(defn find-patt [path]
  (->> (mapcat #(partition % 1 path) (range 2 15))
       distinct
       (keep (fn [patt] (filter #{patt} (partition (count patt) 1 path))))
       (filter next)
       (sort-by #(- (count (first %))))
       (map first)))

(defn part-2 [s]
  (let [all-tiles (->> (exec (intcode/parse-input s))
                       :output
                       (map char)
                       (reduce (fn [[tiles [x y]] c] [(cond-> tiles (not (#{\newline \.} c)) (assoc [x y] c))
                                                      (if (= \newline c) [0 (inc y)] [(inc x) y])])
                               [{} [0 0]])
                       first)
        start-pos (ffirst (filter (comp #{\^} second) all-tiles))
        tiles (dissoc all-tiles start-pos)
        _ (doseq [l (draw-screen all-tiles)] (println l))
        path (find-path tiles [:left :left] start-pos)
        patterns (find-patt path)
        _ (prn (count patterns) patterns)]
    (->> (for [a patterns b patterns c patterns
               :when (and (> (count path) (+ (count a) (count b) (count c))) (distinct? a b c))]
           (let [used {a :a b :b c :c}
                 s (reduce (fn [ps l]
                             (map #(let [m (get used % %)]
                                     (if (coll? m) (first m) m))
                                  (partition-all l 1 ps)))
                           path
                           (distinct (map count [a b c])))]
             ;[(- (count (filter keyword? s))) s]
             (when (not-any? number? s) [s used])
             ))
         (filter identity)
         ;distinct
         ;(sort-by first)
         first)))

(comment
  (part-2 input)
  )
