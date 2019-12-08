(ns day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day7.txt")))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-input [s]
  (mapv parse-int (str/split (str/trim s) #"\s?,\s?")))

(def opcodes {1 [:store + 2]
              2 [:store * 2]
              3 [:store :input 0]
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

(defmulti do-opcode (fn [[type] _] type))

(defmethod do-opcode :default [op state]
  (-> state
      (dissoc :i-ptr)
      (assoc :unknown-op op)))

(defmethod do-opcode :store [[_ f n] {:keys [memory i-ptr input] :as state}]
  (let [next-ptr (+ 1 i-ptr n)
        [op & ns] (subvec memory i-ptr next-ptr)
        [x input] (if (= :input f)
                    [(first input) (next input)]
                    [(apply f (get-args memory op n ns)) input])]
    ;(prn :store x input)
    (when (nil? x)
      (throw (ex-info "cannot store nil" {:op :store :args ns :state state})))
    (-> state
        (assoc-in [:memory (get memory next-ptr)] x)
        (assoc :i-ptr (inc next-ptr) :input input))))

(defmethod do-opcode :output [_ {:keys [memory i-ptr output] :or {output []} :as state}]
  (let [next-ptr (+ i-ptr 1)
        [op & ns] (subvec memory i-ptr (inc next-ptr))
        x (first (get-args memory op 1 ns))]
    ;(prn :output x)
    (when (nil? x)
      (throw (ex-info "cannot output nil" {:op :output :args ns :state state})))
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

(defn exec [state input output]
  (loop [state {:memory (vec state) :i-ptr 0 :input [input output]}]
    (if (int? (:i-ptr state))
      (recur (do-next-opcode state))
      state)))

(defn exec-1 [state inputs]
  (reduce (fn [output input] (first (:output (exec state input output))))
          0
          inputs))

(defn part-1 [state]
  (->> (for [a (range 5) b (range 5) c (range 5) d (range 5) e (range 5) :when (= 5 (count (distinct [a b c d e])))]
         [a b c d e])
       (map #(exec-1 state %))
       (apply max)))

(comment
  (exec-1 (parse-input "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0") [4 3 2 1 0])
  (exec-1 (parse-input "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0") [0 1 2 3 4])
  (exec-1 (parse-input "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0") [1, 0, 4, 3, 2])
  (part-1 (parse-input "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))
  (part-1 (parse-input input))
  )

(defn exec-output [state]
  (loop [state state]
    (if (seq (:output state))
      state
      (if (int? (:i-ptr state))
        (recur (do-next-opcode state))
        state))))

(defn exec-2 [memory inputs]
  (loop [states (map (fn [i]  {:memory (vec memory) :i-ptr 0 :input [i]}) inputs) output 0]
    ;(prn states)
    (let [[state & states] states
          state (update state :input concat [output])
          ;_ (prn (:input state) (last states))
          #_#__ (Thread/sleep 10)]
      (let [state (exec-output state)]
        (if (int? (:i-ptr state))
          (recur (concat states [(dissoc state :output)]) (first (:output state)))
          output)))))

(defn part-2 [state]
  (->> (for [a (range 5 10) b (range 5 10) c (range 5 10) d (range 5 10) e (range 5 10) :when (= 5 (count (distinct [a b c d e])))]
         [a b c d e])
       (map #(exec-2 state %))
       (apply max)))

(comment
  (exec-2 (parse-input "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5") [9 8 7 6 5])
  (exec-2 (parse-input "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
          [9 7 8 5 6])
  (part-2 (parse-input input))                              ;89603079
  )
