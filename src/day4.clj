(ns day4)

(def input (range 172851 675869))

(defn valid-password [s]
  (let [is (map int (str s))]
    (and (apply <= is)
         (some (fn [[a b]] (= a b)) (partition 2 1 is)))))

(defn part-1 []
  (count (filter valid-password input)))

(comment
  (valid-password 111111)
  (valid-password 123789)
  (part-1)                                                  ;1660
  )

(defn valid-password2 [s]
  (let [is (map int (str s))]
    (and (apply <= is)
         (some (fn [[a b c]] (and (= a b) (not= a c)))
               (partition-by identity is)))))

(defn part-2 []
  (count (filter valid-password2 input)))

(comment
  (valid-password2 112233)
  (valid-password2 123444)
  (valid-password2 111122)
  (part-2)                                                  ;1135
  )
