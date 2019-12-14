(ns day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day14.txt")))

(defn parse-int [s]
  (Long/parseLong s))

(defn s->amount [s]
  (let [[n u] (str/split s #"\s+")]
    [(keyword u) (parse-int n)]))

(defn parse-reaction [inputs output]
  (let [[unit n] (s->amount output)]
    {:inputs (map s->amount (str/split inputs #"\s?,\s?"))
     :unit unit
     :n n}))

(defn parse-input [s]
  (->> (str/split-lines (str/trim s))
       (map #(apply parse-reaction (str/split % #"\s?=>\s?")))
       (into {} (map (juxt :unit identity)))))

(defn next-up [a b]
  (quot (+ a (dec b)) b))

(defn amounts [reactions units needed-fn]
  (let [u->prio (into {:ORE 0}
                      (map (fn [{:keys [unit inputs]}]
                             [unit (->> inputs
                                        (iterate (fn [is] (mapcat #(-> % first reactions :inputs) is)))
                                        (take-while seq)
                                        (mapcat identity)
                                        count
                                        -)]))
                      (vals reactions))]
    (loop [reactions reactions units units amounts {}]
      (if-let [{:keys [inputs unit] un :n} (get reactions (first units))]
        (do
          (let [inputs (sort-by (comp u->prio first) inputs)
                needed (needed-fn (get amounts unit 1) un)]
            (recur reactions
                   (sort-by u->prio (distinct (concat (next units) (map first inputs))))
                   (reduce (fn [am [unit n]]
                             (update am unit (fnil + 0) (* needed n))) amounts inputs))))
        (if (seq (next units))
          (recur reactions (next units) amounts)
          amounts)))))

(defn part-1 [s]
  (:ORE (amounts (parse-input s) [:FUEL] next-up))
  )

(comment
  (part-1 "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL")
  (part-1 "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL")
  (part-1 input)                                            ;598038
  )

(defn part-2 [s]
  (quot 1000000000000 (:ORE (amounts (parse-input s) [:FUEL] /)))
  )

(comment
  (part-2 "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")
  (part-2 input)                                            ;2269325
  )
