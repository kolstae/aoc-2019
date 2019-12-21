(ns dijkstra)

(defn map-vals [m f] (into {} (for [[k v] m] [k (f v)])))
(defn remove-keys [m pred] (select-keys m (filter (complement pred) (keys m))))

(defn q-map [k n] {:m {k n} :q (list k)})
(defn q-peek [{:keys [m q]}] (find m (first q)))
(defn q-pop [{:keys [m q]}] (let [k (first q)] {:m (dissoc m k) :q (next q)}))
(defn q-merge-with [f {:keys [m]} m2]
  (let [m (merge-with f m m2)]
    {:m m :q (map first (sort-by val m))}))

(defn dijkstra [start f done?]
  (loop [q (q-map start 0) r {}]
    (if-let [[v d] (q-peek q)]
      (let [dist (-> (f v) (remove-keys r) (map-vals (partial + d)))]
        ;(prn :dijkstra v d dist)
        ;(Thread/sleep 100)
        (if (done? v)
          d
          (recur (q-merge-with min (q-pop q) dist) (assoc r v d))))
      r)))
