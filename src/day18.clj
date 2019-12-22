(ns day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [dijkstra :refer [dijkstra]]))

(def input (slurp (io/resource "day18.txt")))

(defn parse-input [s]
  (into {}
        (mapcat (fn [y line] (remove (comp #{\#} second) (map (fn [x c] [[x y] c]) (range) line)))
                (range)
                (str/split-lines s))))

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

(defn print-screen [tiles]
  (doseq [l (draw-screen tiles)] (println l)))

(defn key? [c] (when (and (char? c) (Character/isLowerCase (char c))) c))
(defn branch-point? [c] (or (key? c) (= \+ c)))
(defn take-until [p s] (transduce (halt-when p (fn [r h] (conj r h))) conj [] s))

(def deltas [[0 -1] [1 0] [0 1] [-1 0]])

(defn poi [tiles delta pos]
  (when-let [t (tiles pos)]
    (cond
      (not= \. t) t
      (let [d (reverse delta)]
        (or (tiles (mapv + pos d))
            (tiles (mapv - pos d)))) \+
      :else false)))

(defn strait-path [tiles pos delta]
  (->> (mapv + pos delta)
       (iterate #(mapv + % delta))
       (map (juxt #(poi tiles delta %) identity))
       (take-while (comp some? first))))

(defn split-paths [keys path]
  (let [[p [[c pos]]] (split-with (fn [[poi]] (or (contains? keys poi) (false? poi))) path)]
    ;(prn p c pos)
    (when (branch-point? c)
      [pos (+ 1 (count p))])))

(defn paths [tiles x y keys]
  ;(prn (filter seq (map #(strait-path tiles [x y] %) deltas)))
  (->> (map #(strait-path tiles [x y] %) deltas)
       (filter seq)
       (keep #(split-paths keys %))))

(defn part-1 [s]
  (let [tiles (parse-input s)
        start-pos (ffirst (filter (comp #{\@} second) tiles))
        all-keys (into #{} (comp (filter (comp key? second))
                                 (map #(Character/toUpperCase (char (second %)))))
                       tiles)
        _ (prn :start-pos start-pos :all-keys all-keys)
        _ (print-screen tiles)
        tiles (assoc tiles start-pos \.)
        paths (fn [[x y keys]]
                (into {}
                      (map (fn [[k v]]
                             (if-let [key (key? (tiles k))]
                               [(conj k (conj keys (Character/toUpperCase (char key)))) v]
                               [(conj k keys) v])))
                      (paths tiles x y keys)))
        ]
    (time (dijkstra (conj start-pos #{}) paths (fn [[_ _ ks]] (= ks all-keys))))
    #_[(paths (conj start-pos #{}))
       (paths [17 1 #{\A}])]
    ))

(comment
  (part-1 "#########\n#b.A.@.a#\n#########")
  (part-1 "########################\n#f.D.E.e.C.b.A.@.a.B.c.#\n######################.#\n#d.....................#\n########################")
  (part-1 "########################\n#...............b.C.D.f#\n#.######################\n#.....@.a.B.c.d.A.e.F.g#\n########################")
  (part-1 "#################\n#i.G..c...e..H.p#\n########.########\n#j.A..b...f..D.o#\n########@########\n#k.E..a...g..B.n#\n########.########\n#l.F..d...h..C.m#\n#################")
  (part-1 "########################\n#@..............ac.GI.b#\n###d#e#f################\n###A#B#C################\n###g#h#i################\n########################")
  (part-1 input)                                            ;1420081
  )
