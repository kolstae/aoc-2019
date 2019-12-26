(ns day21
  (:require [clojure.java.io :as io]
            [intcode]
            [clojure.string :as str]))

(def input (slurp (io/resource "day21.txt")))

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

(defn exec [memory cmds]
  (let [initial-state {:memory memory :i-ptr 0 :input (mapv int (str/join (interleave cmds (repeat "\n")))) :relbase 0}]
    (loop [state initial-state]
      (if (int? (get state :i-ptr))
        (recur (intcode/do-next-opcode state))
        state))))

(defn execute [s cmds]
  (let [cs (:output (exec (intcode/parse-input s) cmds))
        tiles (->> (map #(if (>= (int Character/MAX_VALUE) %) (char %) %) cs)
                   (reduce (fn [[tiles [x y]] c] [(assoc tiles [x y] c)
                                                  (if (= \newline c) [0 (inc y)] [(inc x) y])])
                           [{} [0 0]])
                   first)
        _ (print-screen tiles)
        ]
    (second (first (filter (comp int? second) tiles)))))

(defn part-1 [s cmds]
  (execute s (concat cmds ["WALK"])))

(comment
  (part-1 input ["NOT A J" "NOT B T" "OR T J" "NOT C T" "OR T J" "AND D J"]) ;19353565
  )

(defn part-2 [s cmds]
  (execute s (concat cmds ["RUN"])))


(comment
  (part-2 input ["NOT A J" "NOT B T" "OR T J" "NOT C T" "OR T J" "AND D J"
                 "NOT E T" "NOT T T" "OR H T" "AND T J"])
  )
; ABCDEFGHI
; ##.#.##.##.#
