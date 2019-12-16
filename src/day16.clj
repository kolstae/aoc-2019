(ns day16
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (slurp (io/resource "day16.txt")))

(defn parse-input [s]
  (map #(- (int %) (int \0)) (str/trim s)))

(defn base-pattern [p ns]
  (reduce (fn [sum n] (if n (+ sum n) sum))
          0
          (map (fn [f coll] (when f (reduce f 0 coll))) (cycle [nil + nil -]) (partition-all p (cons 0 ns)))))

(defn fft [ns]
  (map (fn [p] (Math/abs (rem (base-pattern p ns) 10)))
       (range 1 (inc (count ns)))))

(defn part-1 [n s]
  (let [ns (parse-input s)]
    (apply str (take 8 (first (drop n (iterate fft ns)))))))

(comment
  (part-1 4 "12345678")
  (part-1 100 "80871224585914546619083218645595")
  (part-1 100 "19617804207202209144916044189917")
  (part-1 100 "69317163492948606335995924319873")
  (part-1 100 input)                                        ;49254779
  )

(defn fft-rev [[n & ns]]
  (loop [res [n] ns ns p n]
    (if (seq ns)
      (let [n (rem (+ (first ns) p) 10)]
        (recur (conj res n) (next ns) n))
      res)))

(defn part-2 [n s]
  (let [ns (parse-input s)
        offset (Long/parseLong (apply str (take 7 ns)))
        ns-rev (take (- (* 10000 (count ns)) offset) (mapcat identity (repeat 10000 (reverse ns))))]
    (apply str (take 8 (reverse (first (drop n (iterate fft-rev ns-rev))))))))

(comment
  (part-2 100 "03036732577212944063491565474664")
  (part-2 100 "02935109699940807407585447034323")
  (part-2 100 "03081770884921959731165446850517")
  (part-2 100 input)                                        ;55078585
  )
