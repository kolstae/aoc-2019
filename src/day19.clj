(ns day19
  (:require [clojure.java.io :as io]
            [intcode]))

(def input (slurp (io/resource "day19.txt")))

(defn exec-fn [memory]
  (let [initial-state {:memory memory :i-ptr 0 :input [] :relbase 0}]
    (fn [pos]
      (loop [state (assoc initial-state :input pos)]
        (if (seq (:output state))
          (first (:output state))
          (recur (intcode/do-next-opcode state)))))))

(defn part-1 [s]
  (let [test-pos (exec-fn (intcode/parse-input s))]
    (count (for [x (range 50) y (range 50) :when (= 1 (test-pos [x y]))] 1))))

(comment
  (part-1 input)                                            ;166
  )

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

(defn part-2 [s]
  (let [test-pos (exec-fn (intcode/parse-input s))
        xs-for (fn [xx y] [(last (for [x (range xx 0 -1) :while (= 1 (test-pos [x y]))] x))
                           (last (for [x (range xx 10000) :while (= 1 (test-pos [x y]))] x))])
        ;tiles (into {}
        ;            (comp
        ;              (mapcat identity)
        ;              (map (juxt identity (constantly \#))))
        ;            (for [y (range 980 1090) :let [x (- y 610)]] (map #(vector % y) (apply range (update (xs-for x y) 1 inc)))))
        ;tiles (-> tiles
        ;          (update [379 981] #({\# \.} % \¤))
        ;          (update [379 1080] #({\# \.} % \¤))
        ;          (update [478 981] #({\# \.} % \¤))
        ;          (update [478 1080] #({\# \.} % \¤)))
        ;_ (print-screen tiles)
        y 900
        [d1 d2] (xs-for 350 y)
        n-y (int (/ (+ 98 (* 98 (/ d1 (dec y)))) (/ (- d2 d1) y)))
        [y [_ x2]] (->> [n-y (xs-for (int (* (/ (+ d1 d2) 2 y) n-y)) n-y)]
                        (iterate (fn [[y [d1 d2]]]
                                   (let [n-y (inc y)]
                                     [n-y (xs-for (int (* (/ (+ d1 d2) 2 y) n-y)) n-y)])))
                        (map (fn [[y [x1 x2]]]
                               [y [x1 x2]
                                (= 1 (test-pos [(- x2 99) (+ y 99)]) (test-pos [x2 y])
                                   (test-pos [(- x2 99) y]) (test-pos [x2 (+ 99 y)]))]))
                        (drop-while #(false? (nth % 2)))
                        (first))]
    (+ y (* (- x2 99) 10000))))

(comment
  (part-2 input)                                            ;3790981
  )
