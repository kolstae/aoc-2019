(ns day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

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

(defn key? [c] (and (char? c) (Character/isLowerCase (char c))))
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

(defn viable [keys turned-at path]
  (->> path
       (drop-while (fn [[poi]] (or (contains? keys poi) (false? poi))))
       first
       ((fn [[c pos]] (and (branch-point? c) (not (turned-at pos)))))))

(defn paths-from [tiles pos keys turned-at]
  (filter #(viable keys turned-at %) (map #(strait-path tiles pos %) deltas)))

(defn consume-path [{:keys [tiles path walked-path keys all-keys turned-at]}]
  (let [p1 (take-until (comp branch-point? first) path)
        [poi pos] (first (filter (comp branch-point? first) p1))
        tiles (reduce #(assoc %1 %2 \.) tiles (map second p1))
        keys (cond-> keys (key? poi) (conj (Character/toUpperCase (char poi))))
        ;_ (prn pos keys (paths-from tiles pos keys turned-at))
        ;_ (prn all-keys keys)
        ;_ (print-screen tiles)
        walked-path (into walked-path (map second) p1)
        turned-at (if (key? poi) #{} (conj turned-at pos))]
    (if (= all-keys keys)
      {:walked-path walked-path}
      (map (fn [p] {:tiles tiles :keys keys
                    :walked-path walked-path
                    :path p :all-keys all-keys
                    :turned-at turned-at})
           (paths-from tiles pos keys turned-at)))))

(defn part-1 [s]
  (let [tiles (parse-input s)
        start-pos (ffirst (filter (comp #{\@} second) tiles))
        all-keys (into #{} (comp (filter (comp key? second))
                                 (map #(Character/toUpperCase (char (second %)))))
                       tiles)
        _ (prn :start-pos start-pos :all-keys all-keys)
        _ (print-screen tiles)
        m {:tiles (assoc tiles start-pos \.) :keys #{}
           :walked-path [] :all-keys all-keys :turned-at #{start-pos}}]
    (loop [paths (map #(assoc m :path %) (paths-from tiles start-pos #{} #{})) done nil]
      (if (seq paths)
        (let [ps (consume-path (first paths))]
          ;(prn ps)
          (if (map? ps)
            (recur (next paths) (if done (min-key count (:walked-path ps) done) (:walked-path ps)))
            (recur (sort-by (comp count :walked-path) (concat (next paths) ps)) done)))
        (count done)))))

(comment
  (part-1 "#########\n#b.A.@.a#\n#########")
  (part-1 "########################\n#f.D.E.e.C.b.A.@.a.B.c.#\n######################.#\n#d.....................#\n########################")
  (part-1 "########################\n#...............b.C.D.f#\n#.######################\n#.....@.a.B.c.d.A.e.F.g#\n########################")
  (part-1 "#################\n#i.G..c...e..H.p#\n########.########\n#j.A..b...f..D.o#\n########@########\n#k.E..a...g..B.n#\n########.########\n#l.F..d...h..C.m#\n#################")
  (part-1 "########################\n#@..............ac.GI.b#\n###d#e#f################\n###A#B#C################\n###g#h#i################\n########################")
  (part-1 input)
  )
