(ns day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [dijkstra :refer [dijkstra]]))

(def input (slurp (io/resource "day20.txt")))

(defn parse-input [s]
  (into {}
        (mapcat (fn [y line] (remove (comp #{\# \space} second) (map (fn [x c] [[x y] c]) (range) line)))
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

(defn split-paths [n path]
  (loop [path path kvs [] n n]
    (if (some (comp first) path)
      (let [[p ps] (split-with (comp not first) path)
            cnt (+ 1 (count p) n)]
        ;(prn p (first ps) cnt (next ps))
        (recur (next ps) (conj kvs [(second (first ps)) cnt]) cnt))
      kvs)))

(defn paths [tiles pos jump-pos]
  (->> (map #(strait-path tiles (or jump-pos pos) %) deltas)
       (filter #(some? (first %)))
       (mapcat #(split-paths (if jump-pos 1 0) %))))

(defn portal [tiles]
  (let [letter? (set (map char (range (int \A) (inc (int \Z)))))]
    (fn [[pos c]]
      (when (letter? c)
        (mapcat (fn [ds]
                  (when-let [[[p1 c1] [p2 c2]] (seq (keep (comp #(find tiles %) #(mapv + pos %)) ds))]
                    (cond
                      (and (= c c1) (= c2 \.)) [[c p2] [p1 nil] [pos nil]]
                      (and (= c c2) (= c1 \.)) [[c p1] [p2 nil] [pos nil]]
                      (and (= c1 \.) (letter? c2)) [[p1 (keyword (str c c2))] [p2 nil] [pos nil]]
                      (and (= c2 \.) (letter? c1)) [[p2 (keyword (str c1 c))] [p1 nil] [pos nil]])))
                [[[0 -1] [0 1]] [[-1 0] [1 0]]])))))

(defn part-1 [s]
  (let [tiles (parse-input s)
        {portals false endpoints true} (group-by (comp char? first) (mapcat (portal tiles) tiles))
        [start end] (map second (sort-by first endpoints))
        tiles (reduce (fn [ts [p1 p2]] (if p2 (assoc ts p1 p2) (dissoc ts p1))) tiles portals)
        tiles (assoc tiles end \Z)
        _ (prn :start start :end end)
        _ (print-screen tiles)
        portals (into {}
                      (comp
                        (map #(mapv first %))
                        (mapcat (juxt identity (comp vec reverse))))
                      (vals (group-by second (filter second portals))))
        paths (fn [pos]
                (into {} (paths tiles pos (get portals pos))))]
    (dijkstra start paths end)))

(comment
  (part-1 "         A           \n         A           \n  #######.#########  \n  #######.........#  \n  #######.#######.#  \n  #######.#######.#  \n  #######.#######.#  \n  #####  B    ###.#  \nBC...##  C    ###.#  \n  ##.##       ###.#  \n  ##...DE  F  ###.#  \n  #####    G  ###.#  \n  #########.#####.#  \nDE..#######...###.#  \n  #.#########.###.#  \nFG..#########.....#  \n  ###########.#####  \n             Z       \n             Z       ")
  (part-1 "                   A               \n                   A               \n  #################.#############  \n  #.#...#...................#.#.#  \n  #.#.#.###.###.###.#########.#.#  \n  #.#.#.......#...#.....#.#.#...#  \n  #.#########.###.#####.#.#.###.#  \n  #.............#.#.....#.......#  \n  ###.###########.###.#####.#.#.#  \n  #.....#        A   C    #.#.#.#  \n  #######        S   P    #####.#  \n  #.#...#                 #......VT\n  #.#.#.#                 #.#####  \n  #...#.#               YN....#.#  \n  #.###.#                 #####.#  \nDI....#.#                 #.....#  \n  #####.#                 #.###.#  \nZZ......#               QG....#..AS\n  ###.###                 #######  \nJO..#.#.#                 #.....#  \n  #.#.#.#                 ###.#.#  \n  #...#..DI             BU....#..LF\n  #####.#                 #.#####  \nYN......#               VT..#....QG\n  #.###.#                 #.###.#  \n  #.#...#                 #.....#  \n  ###.###    J L     J    #.#.###  \n  #.....#    O F     P    #.#...#  \n  #.###.#####.#.#####.#####.###.#  \n  #...#.#.#...#.....#.....#.#...#  \n  #.#####.###.###.#.#.#########.#  \n  #...#.#.....#...#.#.#.#.....#.#  \n  #.###.#####.###.###.#.#.#######  \n  #.#.........#...#.............#  \n  #########.###.###.#############  \n           B   J   C               \n           U   P   P               ")
  (part-1 input)                                            ;668
  )

(defn outside-pred [tiles]
  (let [xs (map first (keys tiles))
        [min-x max-x] [(apply min xs) (apply max xs)]
        ys (map second (keys tiles))
        [min-y max-y] [(apply min ys) (apply max ys)]]
    (fn [[x y]] (or (#{max-x min-x} x) (#{max-y min-y} y)))))

(defn part-2 [s]
  (let [tiles (parse-input s)
        {portals false endpoints true} (group-by (comp char? first) (mapcat (portal tiles) tiles))
        [start end] (map second (sort-by first endpoints))
        tiles (reduce (fn [ts [p1 p2]] (if p2 (assoc ts p1 p2) (dissoc ts p1))) tiles portals)
        outside? (outside-pred tiles)
        _ (prn :start start :end end)
        portals (into {}
                      (comp
                        (map #(mapv first %))
                        (mapcat (juxt identity (comp vec reverse))))
                      (vals (group-by second (filter second portals))))
        root-tiles (assoc (into {} (remove (fn [[k v]] (and (keyword? v) (outside? k)))) tiles)
                     end \Z start \.)
        tiles (into {} (remove (comp #{end} key)) tiles)
        ;_ (print-screen root-tiles)
        ;_ (println)
        ;_ (println)
        ;_ (print-screen tiles)
        ;_ (prn :end (find root-tiles end))
        paths (fn [[x y lvl]]
                (let [pos [x y]
                      [lvl j-pos] (if (outside? pos)
                                    (if-let [j (when (pos? lvl) (get portals pos))]
                                      [(dec lvl) j] [lvl nil])
                                    (if-let [j (get portals pos)]
                                      [(inc lvl) j] [lvl nil]))
                      ts (if (pos? lvl) tiles root-tiles)]
                  (into {}
                        (map (fn [[pos n]] [(conj pos lvl) n]))
                        (paths ts [x y] j-pos))))]
    (dijkstra (conj start 0) paths (conj end 0))))

(comment
  (part-2 "         A           \n         A           \n  #######.#########  \n  #######.........#  \n  #######.#######.#  \n  #######.#######.#  \n  #######.#######.#  \n  #####  B    ###.#  \nBC...##  C    ###.#  \n  ##.##       ###.#  \n  ##...DE  F  ###.#  \n  #####    G  ###.#  \n  #########.#####.#  \nDE..#######...###.#  \n  #.#########.###.#  \nFG..#########.....#  \n  ###########.#####  \n             Z       \n             Z       ")
  (part-2 "             Z L X W       C                 \n             Z P Q B       K                 \n  ###########.#.#.#.#######.###############  \n  #...#.......#.#.......#.#.......#.#.#...#  \n  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  \n  #.#...#.#.#...#.#.#...#...#...#.#.......#  \n  #.###.#######.###.###.#.###.###.#.#######  \n  #...#.......#.#...#...#.............#...#  \n  #.#########.#######.#.#######.#######.###  \n  #...#.#    F       R I       Z    #.#.#.#  \n  #.###.#    D       E C       H    #.#.#.#  \n  #.#...#                           #...#.#  \n  #.###.#                           #.###.#  \n  #.#....OA                       WB..#.#..ZH\n  #.###.#                           #.#.#.#  \nCJ......#                           #.....#  \n  #######                           #######  \n  #.#....CK                         #......IC\n  #.###.#                           #.###.#  \n  #.....#                           #...#.#  \n  ###.###                           #.#.#.#  \nXF....#.#                         RF..#.#.#  \n  #####.#                           #######  \n  #......CJ                       NM..#...#  \n  ###.#.#                           #.###.#  \nRE....#.#                           #......RF\n  ###.###        X   X       L      #.#.#.#  \n  #.....#        F   Q       P      #.#.#.#  \n  ###.###########.###.#######.#########.###  \n  #.....#...#.....#.......#...#.....#.#...#  \n  #####.#.###.#######.#######.###.###.#.#.#  \n  #.......#.......#.#.#.#.#...#...#...#.#.#  \n  #####.###.#####.#.#.#.#.###.###.#.###.###  \n  #.......#.....#.#...#...............#...#  \n  #############.#.#.###.###################  \n               A O F   N                     \n               A A D   M                     ")
  (part-2 input)                                            ;7778
  )
