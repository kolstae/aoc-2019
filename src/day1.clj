(ns day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-int [s]
  (Integer/parseInt s))

(defn req-fuel [mass]
  (- (long (/ mass 3.0)) 2))

(def input (->> (io/resource "day1.txt")
                slurp
                str/split-lines
                (map parse-int)))

(defn part-1 []
  (reduce + (map req-fuel input)))

(comment
  (part-1))

(defn req-fuel-ext [mass]
  (reduce + (take-while pos? (iterate req-fuel (req-fuel mass)))))

(defn part-2 []
  (reduce + (map req-fuel-ext input)))

(comment
  (part-2))
