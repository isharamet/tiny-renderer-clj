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

(defn visible? [bc]
  (every? #(not (neg? %)) bc))

(defn z-coord [vertices bc]
  (let [zs (map last vertices)
        zzs (map vector bc zs)]
    (reduce + (map (fn [[a b]] (* a b)) zzs))))

(defn draw-triangle [img vertices color zbuf]
  (let [w (.getWidth img)
        h (.getHeight img)
        [xmin ymin xmax ymax] (bbox vertices w h)]
    (reduce
     (fn [[img zbuf] [x y]]
       (let [bc (barycentric vertices [x y])]
         (if (visible? bc)
           (let [z (z-coord vertices bc)
                 zidx (int (+ x (* y w)))]
             (if (> z (nth zbuf zidx))
               [(image/draw-pixel img x y color) (assoc zbuf zidx z)]
               [img zbuf]))
           [img zbuf])))
     [img zbuf]
     (for [x (range xmin xmax)
           y (range ymin ymax)]
       [x y]))))

