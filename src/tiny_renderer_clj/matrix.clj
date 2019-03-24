(ns tiny-renderer-clj.matrix)

(defn matrix [n m]
  (->> (repeat m 0.0)
       vec
       (repeat n)
       vec))

(defn identity-matrix [n]
  (vec
   (for [i (range 0 n)]
     (vec
      (for [j (range 0 n)]
        (if (= i j) 1.0 0.0))))))

(defn multiply [a b]
  (let [n (count a)]
    (vec
     (for [i (range 0 n)]
       (vec
        (for [j (range 0 (count (nth b i)))]
          (reduce (fn [acc x]
                    (+ acc
                       (* (get-in a [i x]) (get-in b [x j])) ))
                  0.0
                  (range 0 n))))))))

(defn m2v [m]
  (let [[[x] [y] [z] [c]] m]
    [(/ x c) (/ y c) (/ z c)]))

(defn v2m [[x y z]]
  [[x]
   [y]
   [z]
   [1.0]])

(defn update-matrix [m tfs]
  (reduce (fn [acc [k v]] (assoc-in acc k v)) m tfs))

