(ns day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-int [s]
  (Integer/parseInt s))

(def input (slurp (io/resource "day5.txt")))

(defn parse-input [s]
  (mapv parse-int (str/split (str/trim s) #"\s?,\s?")))

(def ^:dynamic *op-arg-input* 9)

(def opcodes {1 [:store + 2]
              2 [:store * 2]
              3 [:store (fn [] *op-arg-input*) 0]
              4 [:output]
              5 [:jump #(not (zero? %))]
              6 [:jump zero?]
              7 [:set-if <]
              8 [:set-if =]
              99 [:exit]})

(defn get-args [memory opcode n args]
  (map (fn [mode n] (if (= 1 (rem mode 10)) n (get memory n)))
       (take n (iterate #(quot % 10) (quot opcode 100)))
       args))

(defmulti do-opcode (fn [[type] state] type))

(defmethod do-opcode :default [op state]
  (-> state
      (dissoc :i-ptr)
      (assoc :unknown-op op)))

(defmethod do-opcode :store [[_ f n] {:keys [memory i-ptr] :as state}]
  (let [next-ptr (+ 1 i-ptr n)
        [op & ns] (subvec memory i-ptr next-ptr)
        x (apply f (get-args memory op n ns))]
    (-> state
        (assoc-in [:memory (get memory next-ptr)] x)
        (assoc :i-ptr (inc next-ptr)))))

(defmethod do-opcode :output [_ {:keys [memory i-ptr output] :or {output []} :as state}]
  (let [next-ptr (+ i-ptr 1)
        [op & ns] (subvec memory i-ptr (inc next-ptr))
        x (first (get-args memory op 1 ns))]
    (-> state
        (assoc :output (conj output x))
        (assoc :i-ptr (inc next-ptr)))))

(defmethod do-opcode :jump [[_ pred] {:keys [memory i-ptr] :as state}]
  (let [next-ptr (+ 2 i-ptr)
        [op & ns] (subvec memory i-ptr (inc next-ptr))
        [x ptr] (get-args memory op 2 ns)]
    (assoc state :i-ptr (if (pred x) ptr (inc next-ptr)))))

(defmethod do-opcode :set-if [[_ check] {:keys [memory i-ptr] :as state}]
  (let [next-ptr (+ 3 i-ptr)
        [op & ns] (subvec memory i-ptr next-ptr)
        [a b] (get-args memory op 2 ns)]
    (-> state
        (assoc-in [:memory (get memory next-ptr)] (if (check a b) 1 0))
        (assoc :i-ptr (inc next-ptr)))))

(defmethod do-opcode :exit [[_] state]
  (dissoc state :i-ptr))

(defn do-next-opcode [{:keys [memory i-ptr] :as state}]
  (let [opcode (get memory i-ptr)]
    (do-opcode (get opcodes (rem opcode 100)) state)))

(defn exec [state]
  (loop [state {:memory (vec state) :i-ptr 0}]
    (if (int? (:i-ptr state))
      (recur (do-next-opcode state))
      state)))

(defn part-1 [state]
  (binding [*op-arg-input* 1]
    (last (:output (exec state)))))

(comment
  (do-next-opcode {:memory [1002 4 3 4 33] :i-ptr 0})
  (exec [1002 4 3 4 33])
  (exec [1101 100 -1 4 0])
  (part-1 (parse-input input))                              ;13346482
  )

(defn exec-2 [input state]
  (binding [*op-arg-input* input]
    (loop [state {:memory (vec state) :i-ptr 0}]
      (if (int? (:i-ptr state))
        (recur (do-next-opcode state))
        state))))

(defn part-2 [state]
  (last (:output (exec-2 5 state))))

(comment
  (do-next-opcode {:memory [1002 4 3 4 33] :i-ptr 0})
  (exec-2 7 [3 9 8 9 10 9 4 9 99 -1 8])
  (exec-2 8 [3 9 7 9 10 9 4 9 99 -1 8])
  (exec-2 11 [3 3 1108 -1 8 3 4 3 99])
  (exec-2 1 [3 3 1107 -1 8 3 4 3 99])
  (exec-2 1 [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9])
  (exec-2 0 [3 3 1105 -1 9 1101 0 0 12 4 12 99 1])
  (exec-2 9 [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 1106 0 36 98 0 0 1002 21 125 20
             4 20 1105 1 46 104 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99])
  (part-2 (parse-input input))                              ;12111395
  )
