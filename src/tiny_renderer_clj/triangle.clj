(ns tiny-renderer-clj.triangle
  (:require [tiny-renderer-clj.color :as c]
            [tiny-renderer-clj.image :as i]
            [tiny-renderer-clj.vector :as v]))

(defn bbox [vertices width height]
  (let [xsorted (sort-by first vertices)
        ysorted (sort-by second vertices)
        [xmin] (first xsorted)
        [xmax] (last xsorted)
        [_ ymin] (first ysorted)
        [_ ymax] (last ysorted)]
    [(max 0 xmin)
     (max 0 ymin)
     (min width xmax)
     (min height ymax)]))

(defn barycentric [vertices p]
  (let [[[x1 y1] [x2 y2] [x3 y3]] vertices
        [px py] p
        u1 [(- x3 x1) (- x2 x1) (- x1 px)]
        u2 [(- y3 y1) (- y2 y1) (- y1 py)]
        [ux uy uz] (v/cross u1 u2)]
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

(defn scan-range [y [[x0 y0] [x1 y1] [x2 y2]] height]
  (let [second-half (or (> y y1) (= y0 y1))
        segment-height (if second-half (- y2 y1) (- y1 y0))
        ydiff (if second-half (- y2 y) (- y y0))
        a (float (/ ydiff height))
        b (float (/ ydiff segment-height))
        [x0 x1 x2] (if second-half [x2 x1 x0] [x0 x1 x2])
        ax (+ x0 (Math/ceil (* a (- x2 x0))))
        bx (+ x0 (Math/ceil (* b (- x1 x0))))]
    (if (> ax bx) [bx ax] [ax bx])))

(defn draw-triangle-1 [img width height vertices color-fn zbuf]
  (let [[[x0 y0] [x1 y1] [x2 y2] :as vs] (sort-by second vertices)
        height (- y2 y0)]
    (doall
     (pmap
      (fn [y]
        (let [[ax bx] (scan-range y vs height)]
          (doseq [x (range ax (inc bx))]
            (let [bc (barycentric vertices [x y])
                  z (z-coord vertices bc)
                  idx (int (+ x (* y width)))]
              (if (> z (aget ^floats zbuf idx))
                (do
                  (aset-int img idx (color-fn bc))
                  (aset-float zbuf idx z)))))))
      (range y0 y2)))))

(defn draw-triangle [img width height vertices color-fn zbuf]
  (let [[xmin ymin xmax ymax] (bbox vertices width height)]
    (doall
     (pmap
      (fn [[x y :as p]]
        (let [bc (barycentric vertices p)]
          (if (visible? bc)
            (let [z (z-coord vertices bc)
                  idx (int (+ x (* y width)))]
              (if (> z (aget ^floats zbuf idx))
                (do
                  (aset-int img idx (color-fn bc))
                  (aset-float zbuf idx z)))))))
      (for [y (range ymin ymax)
            x (range xmin xmax)]
        [x y])))))
