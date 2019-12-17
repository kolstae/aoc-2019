(ns day11
  (:require [clojure.java.io :as io]
            [intcode]))

(def input (slurp (io/resource "day11.txt")))

(defn move-robot [{:keys [pos direction panels]} [color turn]]
  (let [next-dir (->> (cycle [:up :right :down :left])
                      (drop-while #(not= direction %))
                      (drop (if (zero? turn) 3 1))
                      first)
        [x y] pos
        next-pos (case next-dir
                   :up [x (dec y)]
                   :right [(inc x) y]
                   :down [x (inc y)]
                   :left [(dec x) y])]
    {:panels (assoc panels pos color)
     :direction next-dir
     :pos next-pos}))

(defn exec [state inputs]
  (loop [state {:memory (vec state) :i-ptr 0 :input inputs :relbase 0
                :robot {:pos [0 0] :direction :up :panels {}}}]
    (let [state (if (= 2 (count (:output state)))
                  (let [robot (move-robot (:robot state) (:output state))]
                    (-> state
                        (assoc :robot robot :output []
                               :input [(get (:panels robot) (:pos robot) 0)])))
                  state)]
      (if (int? (:i-ptr state))
        (recur (intcode/do-next-opcode state))
        state))))

(defn part-1 [s]
  (->> (exec (intcode/parse-input s) [0])
       :robot
       :panels
       count))

(comment
  (reduce move-robot
          {:pos [0 0] :direction :up :panels {}}
          [[1 0] [0 0] [1 0] [1 0] [0 1] [1 0] [1 0]])
  (part-1 input)                                            ;1747
  )

(defn part-2 [s]
  (let [lines (->> (exec (intcode/parse-input s) [1])
                   :robot
                   :panels
                   (sort-by (comp second first))
                   (partition-by (comp second first))
                   (map (fn [line] (sort (keep (fn [[[x] color]] (when (pos? color) x)) line)))))
        start (dec (apply min (mapcat identity lines)))
        end (inc (apply max (mapcat identity lines)))]
    (map (fn [line]
           (->> (concat line [end])
                (mapcat (fn [x1 x2]
                          (cond->> (repeat (dec (- x2 x1)) \space)
                            (not= start x1) (cons \#)))
                        (cons start line))
                (apply str)))
      lines)))

(comment
  (part-2 input)                                            ;("####  ##   ##  ###  #  # #  # #    ### "
  )                                                         ; "   # #  # #  # #  # #  # # #  #    #  #"
                                                            ; "  #  #    #    #  # #### ##   #    ### "
                                                            ; " #   #    # ## ###  #  # # #  #    #  #"
                                                            ; "#    #  # #  # # #  #  # # #  #    #  #"
                                                            ; "####  ##   ### #  # #  # #  # #### ### ")
                                                            ;ZCGRHKLB
