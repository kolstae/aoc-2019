(ns day23
  (:require [clojure.java.io :as io]
            [intcode]))

(def input (slurp (io/resource "day23.txt")))

(defn exec [memory]
  (let [nics (map (fn [addr m] (assoc m :addr addr :input [addr]))
                  (range 50) (repeat {:memory memory :i-ptr 0 :relbase 0}))]
    (loop [nics nics packets {}]
      (let [nics (map (fn [{:keys [addr input] :as nic}]
                        (let [input (or (seq (concat input (get packets addr))) [-1])]
                          ;(prn input)
                          (intcode/do-next-opcode (assoc nic :input input))))
                      nics)
            ;nics (map intcode/do-next-opcode nics)
            packets (into (select-keys packets (remove #(<= 0 % 49) (keys packets)))
                          (map (juxt first next))
                          (filter #(= 3 (count %)) (map :output nics)))
            nics (map (fn [{:keys [output] :as nic}] (cond-> nic (= 3 (count output)) (dissoc :output)))
                      nics)
            _ (when (seq packets) (prn packets))
            [_ _ y] (first (filter #(= 255 (first %)) packets))]
        (if y
          y
          (recur nics packets))))))

(defn part-1 [s]
  (exec (intcode/parse-input s)))

(comment
  (part-1 input)
  )
