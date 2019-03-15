(ns tiny-renderer-clj.vector)

(defn multiply [v d]
  (let [[a1 a2 a3] v]
    [(* a1 d) (* a2 d) (* a3 d)]))

(defn dot [v1 v2]
  (let [[a1 a2 a3] v1
        [b1 b2 b3] v2]
    (+ (* a1 b1) (* a2 b2) (* a3 b3))))

(defn cross [v1 v2]
  (let [[a1 a2 a3] v1
        [b1 b2 b3] v2]
    [(- (* a2 b3) (* a3 b2))
     (- (* a3 b1) (* a1 b3))
     (- (* a1 b2) (* a2 b1))]))

(defn add [v1 v2]
  (let [[a1 a2 a3] v1
        [b1 b2 b3] v2]
    [(+ a1 b1) (+ a2 b2) (+ a3 b3)]))

(defn subtract [v1 v2]
  (let [[a1 a2 a3] v1
        [b1 b2 b3] v2]
    [(- a1 b1) (- a2 b2) (- a3 b3)]))

(defn normalize [v]
  (let [[a1 a2 a3] v
        norm (Math/sqrt (+ (* a1 a1) (* a2 a2) (* a3 a3)))
        l 1]
    (multiply v (/ l norm))))

