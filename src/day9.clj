(ns day9
  (:require [clojure.java.io :as io]
            [intcode]))

(def input (slurp (io/resource "day9.txt")))

(defn exec [state inputs]
  (loop [state {:memory (vec state) :i-ptr 0 :input inputs :relbase 0}]
    (if (int? (:i-ptr state))
      (recur (intcode/do-next-opcode state))
      state)))

(defn part-1 [state inputs]
  (:output (exec state inputs)))

(comment
  (part-1 (intcode/parse-input "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99") [])
  (part-1 (intcode/parse-input "1102,34915192,34915192,7,4,7,99,0") [])
  (part-1 (intcode/parse-input "104,1125899906842624,99") [])
  (part-1 (intcode/parse-input input) [1])                          ;2775723069
  )


(defn part-2 [state inputs]
  (:output (exec state inputs)))

(comment
  (part-2 (intcode/parse-input input) [2])                          ;49115
  )

