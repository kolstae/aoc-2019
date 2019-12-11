(ns day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day11.txt")))

(defn parse-int [s]
  (Long/parseLong s))

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
              9 [:relbase]
              99 [:exit]})

(defn get-args [{:keys [memory relbase]} opcode n args]
  (map (fn [mode n]
         (case (rem mode 10)
           1 n
           2 (get memory (+ relbase n) 0)
           0 (get memory n 0)))
       (take n (iterate #(quot % 10) (quot opcode 100)))
       args))

(defn get-ptr [{:keys [relbase]} opcode skip n]
  (let [[mode] (drop skip (iterate #(quot % 10) (quot opcode 100)))]
    (case (rem mode 10)
      1 (throw (ex-info "illegal mode for storeage" {:mode mode :n n :opcode opcode}))
      2 (+ relbase n)
      0 n)))

(defn update-mem [mem pos x]
  (cond-> mem
    (>= pos (count mem)) (-> (concat (repeat (inc (- pos (count mem))) 0)) vec)
    :update (assoc pos x)))

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
                    [(apply f (get-args state op n ns)) input])]
    ;(prn :store x input)
    (when (nil? x)
      (throw (ex-info "cannot store nil" {:op :store :args ns :state state})))
    (-> state
        (update :memory update-mem (get-ptr state op n (get memory next-ptr)) x)
        (assoc :i-ptr (inc next-ptr) :input input))))

(defmethod do-opcode :output [_ {:keys [memory i-ptr output] :or {output []} :as state}]
  (let [next-ptr (+ i-ptr 2)
        [op & ns] (subvec memory i-ptr next-ptr)
        [x] (get-args state op 1 ns)]
    ;(prn :output x)
    (when (nil? x)
      (throw (ex-info "cannot output nil" {:op :output :args ns :state state})))
    (-> state
        (assoc :output (conj output x))
        (assoc :i-ptr next-ptr))))

(defmethod do-opcode :jump [[_ pred] {:keys [memory i-ptr] :as state}]
  (let [next-ptr (+ 3 i-ptr)
        [op & ns] (subvec memory i-ptr next-ptr)
        [x ptr] (get-args state op 2 ns)]
    (assoc state :i-ptr (if (pred x) ptr next-ptr))))

(defmethod do-opcode :set-if [[_ check] {:keys [memory i-ptr] :as state}]
  (let [next-ptr (+ 3 i-ptr)
        [op & ns] (subvec memory i-ptr next-ptr)
        [a b] (get-args state op 2 ns)]
    (-> state
        (update :memory update-mem (get-ptr state op 2 (get memory next-ptr)) (if (check a b) 1 0))
        (assoc :i-ptr (inc next-ptr)))))

(defmethod do-opcode :relbase [_ {:keys [memory i-ptr] :as state}]
  (let [next-ptr (+ 2 i-ptr)
        [op & ns] (subvec memory i-ptr next-ptr)
        [x] (get-args state op 1 ns)]
    ;(prn :relbase x (:relbase state))
    (-> state
        (update :relbase + x)
        (assoc :i-ptr next-ptr))))

(defmethod do-opcode :exit [[_] state]
  (dissoc state :i-ptr))

(defn do-next-opcode [{:keys [memory i-ptr] :as state}]
  (let [opcode (get memory i-ptr)]
    (do-opcode (get opcodes (rem opcode 100)) state)))

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
        (recur (do-next-opcode state))
        state))))

(defn part-1 [s]
  (->> (exec (parse-input s) [0])
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
  (let [lines (->> (exec (parse-input s) [1])
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
