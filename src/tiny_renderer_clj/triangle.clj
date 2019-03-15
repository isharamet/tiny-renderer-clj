(ns tiny-renderer-clj.triangle
  (:require [tiny-renderer-clj.color :as color]
            [tiny-renderer-clj.image :as image]
            [tiny-renderer-clj.vector :as vector]))

(defn bbox [vertices width height]
  (let [xsorted (sort-by first vertices)
        ysorted (sort-by second vertices)
        [xmin] (first xsorted)
        [xmax] (last xsorted)
        [_ ymin] (first ysorted)
        [_ ymax] (last ysorted)]
    [xmin ymin xmax ymax]))

(defn barycentric [vertices p]
  (let [[[x1 y1] [x2 y2] [x3 y3]] vertices
        [px py] p
        u1 [(- x3 x1) (- x2 x1) (- x1 px)]
        u2 [(- y3 y1) (- y2 y1) (- y1 py)]
        [ux uy uz] (vector/cross u1 u2)]
    (if (zero? (Math/abs uz))
      [-1 1 1]
      (let [u (- 1.0 (/ (+ ux uy) uz))
            v (float (/ uy uz))
            w (float (/ ux uz))]
        [u v w]))))

(defn visible? [bc]
  (every? #(not (neg? %)) bc))

(defn z-coord [vertices bc]
  (let [zs (map last vertices)
        zzs (map vector bc zs)]
    (reduce + (map (fn [[a b]] (* a b)) zzs))))

(defn draw-triangle [img width height vertices color-fn zbuf]
  (let [[xmin ymin xmax ymax] (bbox vertices width height)]
    (doseq [y (range ymin ymax)
            x (range xmin xmax)]
      (let [bc (barycentric vertices [x y])]
        (if (visible? bc)
          (let [z (z-coord vertices bc)
                idx (int (+ x (* y width)))]
            (if (> z (aget ^floats zbuf idx))
              (do
                (aset-int img idx (color-fn bc))
                (aset-float zbuf idx z)))))))))
