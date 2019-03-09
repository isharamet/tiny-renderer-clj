(ns tiny-renderer-clj.triangle
  (:require [tiny-renderer-clj.color :as color]
            [tiny-renderer-clj.image :as image]
            [tiny-renderer-clj.line :as line]
            [tiny-renderer-clj.vector :as vector])
  (:import (java.awt Color)))

(defn bbox [vertices width height]
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
        [ux uy uz] (vector/cross u1 u2)]
    (if (zero? (Math/abs uz))
      [-1 1 1]
      (let [x (- 1.0 (/ (+ ux uy) uz))
            y (float (/ uy uz))
            z (float (/ ux uz))]
        [x y z]))))

(defn draw-triangle [img vertices color zbuf]
  (let [width (.getWidth img)
        height (.getHeight img)
        [xmin ymin xmax ymax] (bbox vertices width height)]
    (reduce
     (fn [[img zbuf] y]
       (reduce
        (fn [[img zbuf] x]
          (let [b (barycentric vertices [x y])
                d (every? #(not (neg? %)) b)]
            (if d
              (let [vzs (map last vertices)
                    z (reduce + (map (fn [[a b]] (* a b)) (map vector b vzs)))
                    zidx (int (+ x (* y width)))]
                (if (> z (nth zbuf zidx))
                  [(image/draw-pixel img x y color)
                   (assoc zbuf zidx z)]
                  [img zbuf]))
              [img zbuf])))
        [img zbuf]
        (range xmin xmax)))
     [img zbuf]
     (range ymin ymax))))

