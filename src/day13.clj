(ns day13
  (:require [clojure.java.io :as io]
            [intcode]))

(def input (slurp (io/resource "day13.txt")))

(defn exec [state]
  (loop [state {:memory (vec state) :i-ptr 0 :input [] :relbase 0}]
    (if (int? (:i-ptr state))
      (recur (intcode/do-next-opcode state))
      state)))

(defn part-1 [s]
  (->> (exec (intcode/parse-input s))
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
        (recur (intcode/do-next-opcode (update state :input #(if (seq %) % [0]))))
        state))))

(defn part-2 [s]
  (-> (intcode/parse-input s)
      (assoc 0 2)
      play
      :screen
      :score
      #_draw-screen))

(comment
  (part-2 input)                                            ;13824
  )
