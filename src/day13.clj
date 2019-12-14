(ns day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day13.txt")))

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
    (when (nil? x)
      (throw (ex-info "cannot store nil" {:op :store :args ns :state state})))
    (-> state
        (update :memory update-mem (get-ptr state op n (get memory next-ptr)) x)
        (assoc :i-ptr (inc next-ptr) :input input))))

(defmethod do-opcode :output [_ {:keys [memory i-ptr output] :or {output []} :as state}]
  (let [next-ptr (+ i-ptr 2)
        [op & ns] (subvec memory i-ptr next-ptr)
        [x] (get-args state op 1 ns)]
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
    (-> state
        (update :relbase + x)
        (assoc :i-ptr next-ptr))))

(defmethod do-opcode :exit [[_] state]
  (dissoc state :i-ptr))

(defn do-next-opcode [{:keys [memory i-ptr] :as state}]
  (let [opcode (get memory i-ptr)]
    (do-opcode (get opcodes (rem opcode 100)) state)))

(defn exec [state]
  (loop [state {:memory (vec state) :i-ptr 0 :input [] :relbase 0}]
    (if (int? (:i-ptr state))
      (recur (do-next-opcode state))
      state)))

(defn part-1 [s]
  (->> (exec (parse-input s))
       :output
       (partition 3)
       (filter #(= 2 (last %)))
       distinct
       count))

(comment
  (part-1 input)                                            ;296
  )

(def tile->char {:paddle \_ :ball \. :wall \# :block \-})

(defn draw-screen [{:keys [pixels score]}]
  (->> pixels
       (sort-by (comp second first))
       (partition-by (comp second first))
       (map (fn [line] (->> (sort-by first line)
                            (map (fn [[[x] tile]] [x (get tile->char tile)]))
                            (partition-all 2 1)
                            (mapcat (fn [[[x c] [x2]]]
                                      (cons c (when x2 (repeat (dec (- x2 x)) \space)))))
                            (apply str))))
       (cons (str "Score: " score))))

(defn sign [n]
  (if (pos? n) 1 (if (zero? n) 0 -1)))

(def tiles [nil :wall :block :paddle :ball])

(defn x-velocity [[x y] [xx yy]]
  [(- xx x) (- yy y)])


(defn update-paddle [{:keys [ball paddle] :as movement} tile pos]
  (cond-> (assoc movement tile pos)
    (and ball paddle (= :ball tile)) (assoc :paddle-target-x (let [[_ py] paddle
                                                                   [x y] pos
                                                                   [x-v x-y] (x-velocity ball pos)]
                                                               (if (pos? x-y) (+ x (* (- py y) x-v)) x)))))

(defn update-screen [screen [x y tile]]
  (cond
    (= -1 x) (assoc screen :score tile)
    (zero? tile) (update screen :pixels dissoc [x y])
    :else (let [tile (get tiles tile)]
            (cond-> (assoc-in screen [:pixels [x y]] tile)
              (#{:paddle :ball} tile) (update :movement update-paddle tile [x y])))))

(defn play [state]
  (loop [state {:memory (vec state) :i-ptr 0 :input [] :relbase 0}]
    (let [state (if (= 3 (count (:output state)))
                  (let [screen (update-screen (:screen state) (:output state))
                        ;_ (when (neg? (first (:output state))) (prn (draw-screen screen)))
                        x (get-in screen [:movement :paddle 0] 0)
                        input (sign (- (get-in screen [:movement :paddle-target-x] x) x))]
                    (assoc state :screen screen :output [] :input [input]))
                  state)]
      (if (int? (:i-ptr state))
        (recur (do-next-opcode (update state :input #(if (seq %) % [0]))))
        state))))

(defn part-2 [s]
  (-> (parse-input s)
      (assoc 0 2)
      play
      :screen
      :score
      #_draw-screen))

(comment
  (part-2 input)                                            ;13824
  )
