(ns day15
  (:require [clojure.java.io :as io]
            [intcode]))

(def input (slurp (io/resource "day15.txt")))

(def tile->char {:empty \. :wall \# :start \* :oxygen \@})

(defn draw-screen [{:keys [tiles]}]
  (let [min-x (dec (apply min (map first (keys tiles))))]
    (->> tiles
         (sort-by (comp second first))
         (partition-by (comp second first))
         (map (fn [line] (->> (sort-by first line)
                              (map (fn [[[x] tile]] [x (get tile->char tile)]))
                              (cons [min-x \space])
                              (partition-all 2 1)
                              (mapcat (fn [[[x c] [x2]]]
                                        (cons c (when x2 (repeat (dec (- x2 x)) \space)))))
                              (apply str)))))))

(def directions {:north 1 :south 2 :west 3 :east 4})

(def rel-pos [[[0 -1] :north] [[1 0] :east] [[0 1] :south] [[-1 0] :west]])

(defn find-direction [tiles pos path]
  (if-let [dir (->> (shuffle rel-pos)
                    (remove (comp tiles #(map + % pos) first))
                    (first)
                    (second))]
    [(if (= (first path) pos) path (cons pos path)) dir]
    (let [path (drop-while #{pos} path)
          direction (-> #{(mapv - (first path) pos)} (comp first) (filter rel-pos) first second)]
      [(next path) direction])))

(def ^:dynamic *stop-at-oxygen* true)

(defn move-robot [{:keys [pos direction tiles path] :as m} [status]]
  (let [[x y] pos
        dir-kw (ffirst (filter (comp #{direction} val) directions))
        next-pos (case dir-kw
                   :north [x (dec y)]
                   :east [(inc x) y]
                   :south [x (inc y)]
                   :west [(dec x) y])
        [tile-pos tile next-pos done] (case status
                                        0 [next-pos :wall pos]
                                        1 [next-pos :empty next-pos]
                                        2 [next-pos :oxygen next-pos *stop-at-oxygen*])
        tiles (assoc tiles tile-pos tile)
        [path direction] (find-direction tiles next-pos path)
        done (or done (nil? direction))
        #_#__ (prn done direction *stop-at-oxygen*)]
    (cond-> (assoc m :tiles tiles
                     :direction (get directions direction)
                     :pos next-pos
                     :path path)
      done (assoc :done next-pos))))

(defn exec [memory]
  (let [initial-state {:memory memory :i-ptr 0 :input [2] :relbase 0
                       :robot {:pos [0 0] :direction 2 :tiles {[0 0] :start}}}]
    (loop [state initial-state]
      (let [state (if (seq (:output state))
                    (let [robot (move-robot (:robot state) (:output state))]
                      (-> state
                          (assoc :robot robot :output []
                                 :input [(:direction robot)])))
                    state)]
        (if (not (get-in state [:robot :done]))
          (recur (intcode/do-next-opcode state))
          state)))))

(defn part-1 [s]
  (count (:path (:robot (exec (intcode/parse-input s))))))

(comment
  (part-1 input)                                            ;380
  )

(defn empty-tiles [tiles pos]
  ;(prn pos)
  (->> (map (comp #(mapv + % pos) first) rel-pos)
       (remove tiles)))

(defn fill-oxygen [[tiles ps cnt]]
  ;(prn ps cnt)
  (when (seq ps)
    (let [tiles (into tiles (map #(vector % :oxygen)) ps)]
      [tiles (mapcat #(empty-tiles tiles %) ps) (inc cnt)])))

(defn part-2 [s]
  (binding [*stop-at-oxygen* false]
    (let [tiles (into {} (remove (comp #{:empty :start} second)) (:tiles (:robot (exec (intcode/parse-input s)))))
          [start-pos] (first (filter (comp #{:oxygen} second) tiles))
          filled (last (take-while seq (iterate fill-oxygen [tiles [start-pos] -1])))]
      (doseq [l (draw-screen {:tiles (first filled)})] (println l))
      (last filled))))

(comment
  (part-2 input)                                            ;410
  )
