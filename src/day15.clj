(ns day15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day15.txt")))

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

(def tile->char {:empty \. :wall \# :start \* :oxygen \@})

(defn draw-screen [{:keys [tiles]}]
  (let [min-x (dec (apply min (map first (keys tiles))))]
    (->> tiles
         (sort-by (comp second first))
         (partition-by (comp second first))
         (map (fn [line] (->> (sort-by first line)
                              (map (fn [[[x] tile]] [x (get tile->char tile)]))
                              (cons [min-x \space])
                              (partition-all 2 1)
                              (mapcat (fn [[[x c] [x2]]]
                                        (cons c (when x2 (repeat (dec (- x2 x)) \space)))))
                              (apply str)))))))

(def directions {:north 1 :south 2 :west 3 :east 4})

(def rel-pos [[[0 -1] :north] [[1 0] :east] [[0 1] :south] [[-1 0] :west]])

(defn find-direction [tiles pos path]
  (if-let [dir (->> (shuffle rel-pos)
                    (remove (comp tiles #(map + % pos) first))
                    (first)
                    (second))]
    [(if (= (first path) pos) path (cons pos path)) dir]
    (let [path (drop-while #{pos} path)
          direction (-> #{(mapv - (first path) pos)} (comp first) (filter rel-pos) first second)]
      [(next path) direction])))

(def ^:dynamic *stop-at-oxygen* true)

(defn move-robot [{:keys [pos direction tiles path] :as m} [status]]
  (let [[x y] pos
        dir-kw (ffirst (filter (comp #{direction} val) directions))
        next-pos (case dir-kw
                   :north [x (dec y)]
                   :east [(inc x) y]
                   :south [x (inc y)]
                   :west [(dec x) y])
        [tile-pos tile next-pos done] (case status
                                        0 [next-pos :wall pos]
                                        1 [next-pos :empty next-pos]
                                        2 [next-pos :oxygen next-pos *stop-at-oxygen*])
        tiles (assoc tiles tile-pos tile)
        [path direction] (find-direction tiles next-pos path)
        done (or done (nil? direction))
        #_#__ (prn done direction *stop-at-oxygen*)]
    (cond-> (assoc m :tiles tiles
                     :direction (get directions direction)
                     :pos next-pos
                     :path path)
      done (assoc :done next-pos))))

(defn exec [memory]
  (let [initial-state {:memory memory :i-ptr 0 :input [2] :relbase 0
                       :robot {:pos [0 0] :direction 2 :tiles {[0 0] :start}}}]
    (loop [state initial-state]
      (let [state (if (seq (:output state))
                    (let [robot (move-robot (:robot state) (:output state))]
                      (-> state
                          (assoc :robot robot :output []
                                 :input [(:direction robot)])))
                    state)]
        (if (not (get-in state [:robot :done]))
          (recur (do-next-opcode state))
          state)))))

(defn part-1 [s]
  (count (:path (:robot (exec (parse-input s))))))

(comment
  (part-1 input)                                            ;380
  )

(defn empty-tiles [tiles pos]
  ;(prn pos)
  (->> (map (comp #(mapv + % pos) first) rel-pos)
       (remove tiles)))

(defn fill-oxygen [[tiles ps cnt]]
  ;(prn ps cnt)
  (when (seq ps)
    (let [tiles (into tiles (map #(vector % :oxygen)) ps)]
      [tiles (mapcat #(empty-tiles tiles %) ps) (inc cnt)])))

(defn part-2 [s]
  (binding [*stop-at-oxygen* false]
    (let [tiles (into {} (remove (comp #{:empty :start} second)) (:tiles (:robot (exec (parse-input s)))))
          [start-pos] (first (filter (comp #{:oxygen} second) tiles))
          filled (last (take-while seq (iterate fill-oxygen [tiles [start-pos] -1])))]
      (doseq [l (draw-screen {:tiles (first filled)})] (println l))
      (last filled))))

(comment
  (part-2 input)                                            ;410
  )
