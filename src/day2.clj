(ns day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-int [s]
  (Integer/parseInt s))

(def input (slurp (io/resource "day2.txt")))

(defn parse-input [s]
  (mapv parse-int (str/split (str/trim s) #"\s?,\s?")))

(defn do-opcode [state [op x1 x2 x3]]
  (let [f (case op 1 + 2 *)]
    (assoc state x3 (f (get state x1) (get state x2)))))

(defn do-next-opcode [state idx]
  (let [opcode (get state idx)]
    (if (= opcode 99)
      [state nil]
      [(do-opcode state (subvec state idx (+ idx 4))) (+ idx 4)])))

(defn exec [state noun verb]
  (loop [state (assoc state 1 noun 2 verb)
         idx 0]
    (if (int? idx)
      (let [[state idx] (do-next-opcode state idx)]
        (recur state idx))
      state)))

(defn part-1 [s]
  (exec (parse-input s) 12 2))

(comment
  (first (part-1 input))                                    ;8017076
  )


(defn part-2 []
  (let [state (parse-input input)]
    (for [n (range 100) v (range 100)
          :when (= 19690720 (first (exec state n v)))]
      [n v])))

(comment
  (part-2)                                                  ;([31 46])
  )
