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
    (cond
      (branch-point? c) [pos (+ 1 (count p))]
      (contains? keys (first (last p))) [(second (last p)) (+ (count p))])))

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
    #_(time (dijkstra (conj start-pos #{}) paths (fn [[_ _ ks]] (= ks all-keys))))
    [(paths (conj start-pos #{}))
     #_(paths [17 1 #{\A}])]
    ))

(comment
  (part-1 "#########\n#b.A.@.a#\n#########")
  (part-1 "########################\n#f.D.E.e.C.b.A.@.a.B.c.#\n######################.#\n#d.....................#\n########################")
  (part-1 "########################\n#...............b.C.D.f#\n#.######################\n#.....@.a.B.c.d.A.e.F.g#\n########################")
  (part-1 "#################\n#i.G..c...e..H.p#\n########.########\n#j.A..b...f..D.o#\n########@########\n#k.E..a...g..B.n#\n########.########\n#l.F..d...h..C.m#\n#################")
  (part-1 "########################\n#@..............ac.GI.b#\n###d#e#f################\n###A#B#C################\n###g#h#i################\n########################")
  (part-1 input)                                            ;1420081
  )

(defn part-2 [s]
  (let [tiles (parse-input s)
        start-pos (sort (map first (filter (comp #{\@} second) tiles)))
        all-keys (into #{} (comp (filter (comp key? second))
                                 (map #(Character/toUpperCase (char (second %)))))
                       tiles)
        [tiles start-pos] (if (= (count start-pos) 1)
                            [(reduce (fn [m p] (dissoc m p))
                                     (dissoc tiles (first start-pos))
                                     (map #(mapv + (first start-pos) %) deltas))
                             (map #(mapv + (first start-pos) %) [[-1 -1] [-1 1] [1 -1] [1 1]])]
                            [tiles start-pos])
        _ (prn :start-pos start-pos :all-keys all-keys)
        tiles (into tiles (zipmap start-pos (repeat \.)))
        _ (print-screen tiles)
        paths (fn [[keys & ps]]
                (into {}
                      (mapcat (fn [p]
                                (let [[x y] p]
                                  (map (fn [[k v]]
                                         (let [as (take-while (complement #{p}) ps)
                                               cs (next (drop-while (complement #{p}) ps))]
                                           [(into [(if-let [key (key? (tiles k))]
                                                    (conj keys (Character/toUpperCase (char key)))
                                                    keys)]
                                                 (concat as [k] cs))
                                            v]))
                                       (paths tiles x y keys))))
                              ps)))
        start-pos (into [#{}] start-pos)
        ]
    #_[(paths [#{\E \H \I} [5 2] [5 5] [9 1] [7 6]])
     (paths [#{\E \H \I} [5 2] [5 5] [7 1] [7 6]])
     (paths [#{\E \H \I} [5 2] [5 5] [9 1] [7 7]])]
    (dijkstra start-pos paths (fn [[ks]] (= all-keys ks)))
    ))

(comment
  (part-2 "#######\n#a.#Cd#\n##...##\n##.@.##\n##...##\n#cB#Ab#\n#######")
  (part-2 "###############\n#d.ABC.#.....a#\n######@#@######\n###############\n######@#@######\n#b.....#.....c#\n###############")
  (part-2 "#############\n#DcBa.#.GhKl#\n#.###@#@#I###\n#e#d#####j#k#\n###C#@#@###J#\n#fEbA.#.FgHi#\n#############")
  (part-2 "#############\n#g#f.D#..h#l#\n#F###e#E###.#\n#dCba@#@BcIJ#\n#############\n#nK.L@#@G...#\n#M###N#H###.#\n#o#m..#i#jk.#\n#############")
  (time (part-2 input))
  )
