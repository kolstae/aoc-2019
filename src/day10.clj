(ns day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day10.txt")))

(defn parse-coords [s]
  (->> (str/split-lines s)
       (map-indexed (fn [y line]
                      (->> line
                           (map-indexed (fn [x c] (when-not (= \. c) [x y])))
                           (filter identity))))
       (mapcat identity)))

(defn slope [[x y] [xx yy]]
  (let [dx (- xx x) dy (- yy y)]
    (if (zero? dx)
      [false (if (pos? dy) Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY)]
      [(neg? dx) (/ dy dx)])))

(defn part-1 [s]
  (let [coords (parse-coords s)]
    (->> coords
         (map (fn [a]
                [a (->> coords
                        (remove #{a})
                        (map (juxt identity #(slope a %)))
                        (group-by second))]))
         (map (fn [[k v]] [k (count v)]))
         (sort-by second)
         last)))

(comment
  (part-1 ".#..#\n.....\n#####\n....#\n...##")
  (first (part-1 "#.........\n...A......\n...B..a...\n.EDCG....a\n..F.c.b...\n.....c....\n..efd.c.gb\n.......c..\n....f...c.\n...e..d..c"))
  (part-1 "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####")
  (part-1 "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.")
  (part-1 ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..")
  (part-1 ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
  (part-1 input)                                            ;[[8 16] 214]
  )

(defn dist [[x y] [xx yy]]
  [(Math/abs (- xx x)) (Math/abs (- yy y))])

(defn interleave-all [colls]
  (lazy-seq
    (let [ss (keep seq colls)]
      (when (seq ss)
        (concat (map first ss) (interleave-all (map rest ss)))))))

(defn hit-coords [a s]
  (let [coords (parse-coords s)]
    (->> coords
         (remove #{a})
         (map (juxt #(dist a %) #(slope a %) identity))
         (group-by second)
         (map (fn [[k v]] [k (map #(nth % 2) (sort-by first v))]))
         (sort-by first)
         (map second)
         interleave-all)))

(defn part-2 [hit-coords]
  (->> (drop 199 hit-coords)
       first
       ((juxt #(* 100 (first %)) (comp identity second)))
       (apply +)))

(comment
  (hit-coords [8 3] ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....X...###..\n..#.#.....#....##")
  (part-2 (hit-coords [11 13] ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"))
  (part-2 (hit-coords [8 16] input))                        ;502
  )
