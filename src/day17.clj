(ns day17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
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

(defn exec [memory & [input]]
  (let [initial-state {:memory memory :i-ptr 0 :input (or input []) :relbase 0}]
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

(defn skip-patt [patts path]
  (loop [ps patts path path]
    (if (seq ps)
      (let [[patt & ps] ps]
        (if (= patt (take (count patt) path))
          (recur patts (drop (count patt) path))
          (recur ps path)))
      path)))

(defn enc [cs]
  (->> cs
       (partition-by identity)
       (map (fn [[c & cs]] (if cs (inc (count cs)) c)))
       (str/join \,)))

(defn enc-len [cs]
  (count (enc cs)))

(defn compress [patts->c path]
  (loop [ps patts->c path path cs []]
    (if (seq ps)
      (let [[patt c] (first ps)
            ps (dissoc ps patt)]
        (if (= patt (take (count patt) path))
          (recur patts->c (drop (count patt) path) (conj cs c))
          (recur ps path cs)))
      (str/join \, cs))))

(defn mk-fns [mem]
  (let [all-tiles (->> (exec mem)
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
        c-path (mapcat (fn [x] (if (int? x) (repeat x \f) ({:right '(\R) :left '(\L)} x))) path)
        min-len 3]
    (loop [a min-len b min-len c min-len]
      (let [as (take a c-path)
            rs (skip-patt [as] c-path)
            bs (take b rs)
            rs (skip-patt [as bs] rs)
            cs (take c rs)
            rs (skip-patt [as bs cs] rs)]
        (if (seq rs)
          (let [[a b c] (if (> 20 (enc-len cs))
                          [a b (inc c)]
                          (if (> 20 (enc-len bs))
                            [a (inc b) min-len]
                            [(inc a) min-len min-len]))]
            (recur a b (inc c)))
          [(compress {as \A bs \B cs \C} c-path)
           (enc as)
           (enc bs)
           (enc cs)])))))

(defn part-2 [s]
  (let [mem (intcode/parse-input s)
        fns (mk-fns mem)
        input (str/join (interleave (concat fns ["n"]) (repeat "\n")))
        _ (println input)]
    (->> (mapv int input)
         (exec (assoc mem 0 2))
         :output
         last)))

(comment
  (part-2 input)                                            ;809736
  )
