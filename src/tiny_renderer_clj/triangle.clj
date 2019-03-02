(ns tiny-renderer-clj.triangle
  (:require [tiny-renderer-clj.color :as color]
            [tiny-renderer-clj.image :as image]
            [tiny-renderer-clj.line :as line])
  (:import (java.awt Color)))

(defn draw-triangle-1 [img vertices color]
  (let [sorted-vertices (sort-by second vertices)
        [x0 y0 _] (first sorted-vertices)
        [x1 y1 _] (second sorted-vertices)
        [x2 y2 _] (last sorted-vertices)
        height (- y2 y0)]
    (reduce (fn [img y]
              (let [second-half (> y y1)
                    segment-height (if second-half (- y2 y1) (- y1 y0))
                    ydiff (if second-half (- y2 y) (- y y0))
                    bdiv (if (> segment-height 0) segment-height 1)
                    alpha (double (/ ydiff height))
                    beta (double (/ ydiff bdiv))
                    [x0 x1 x2] (if second-half [x2 x1 x0] [x0 x1 x2])
                    ax (+ x0 (Math/round (* alpha (- x2 x0))))
                    bx (+ x0 (Math/round (* beta (- x1 x0))))]
                (line/draw-line img ax y bx y color)))
            img
            (range y0 y2))))

(defn cross [v1 v2]
  (let [[a1 a2 a3] v1
        [b1 b2 b3] v2]
    [(- (* a2 b3) (* a3 b2))
     (- (* a3 b1) (* a1 b3))
     (- (* a1 b2) (* a2 b1))]))

(defn bbox [vertices]
  (let [xsorted (sort-by first vertices)
        ysorted (sort-by second vertices)
        [xmin _ _] (first xsorted)
        [xmax _ _] (last xsorted)
        [_ ymin _] (first ysorted)
        [_ ymax  ] (last ysorted)]
    [xmin ymin xmax ymax]))

(defn barycentric [vertices p]
  (let [[[x1 y1 _] [x2 y2 _] [x3 y3 _]] vertices
        [px py _] p
        u1 [(- x3 x1) (- x2 x1) (- x1 px)]
        u2 [(- y3 y1) (- y2 y1) (- y1 py)]
        [ux uy uz] (cross u1 u2)]
    (if (zero? (Math/abs uz))
      [-1 1 1]
      (let [x (- 1.0 (/ (+ ux uy) uz))
            y (double (/ uy uz))
            z (double (/ ux uz))]
        [x y z]))))

(defn draw-triangle-2 [img vertices color]
  (let [[xmin ymin xmax ymax] (bbox vertices)]
    (reduce (fn [img y]
              (reduce (fn [img x]
                        (let [b (barycentric vertices [x y])
                              d (every? #(not (neg? %)) b)]
                          (if d (image/draw-pixel img x y color) img)))
                      img
                      (range xmin (inc xmax))))
            img
            (range ymin (inc ymax)))))

(defn draw-triangle 
  ([img vertices color]
   (draw-triangle-2 img vertices color))
  ([img vertices]
   (let [color (color/random-color)]
     (draw-triangle img vertices color))))

