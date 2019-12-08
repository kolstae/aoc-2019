(ns day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day8.txt")))

(defn parse-input [s]
  (map #(- (int %) (int \0)) (str/trim s)))

(defn part-1 [s]
  (let [w 25
        h 6
        most-zeros (->> (parse-input s)
                        (partition (* w h))
                        (sort-by #(count (filter zero? %)))
                        first)]
    (* (count (filter #{1} most-zeros)) (count (filter #{2} most-zeros)))))

(comment
  (parse-input "123456789012")
  (part-1 input)                                            ;2176
  )

(defn part-2 [w h s]
  (->> (parse-input s)
       (partition (* w h))
       (apply map (fn [& ps] (first (keep {1 \# 0 \space} ps))))
       (partition w)
       (map #(apply str %))))

(comment
  (part-2 2 2 "0222112222120000")
  (part-2 25 6 input)                                       ;(" ##  #   ##  # ###  #   #"
                                                            ; "#  # #   ## #  #  # #   #"
                                                            ; "#     # # ##   ###   # # "
                                                            ; "#      #  # #  #  #   #  "
                                                            ; "#  #   #  # #  #  #   #  "
                                                            ; " ##    #  #  # ###    #  ")
  )
