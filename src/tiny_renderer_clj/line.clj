(ns tiny-renderer-clj.line
  (:require [tiny-renderer-clj.image :as image])
  (:import (java.awt Color)))

(defn draw-line-1 [img x0 y0 x1 y1 color]
  (let [next-c (fn [s e t]
                 (Math/round (double (+ s (* (- e s) t)))))]
    (reduce (fn [img t]
              (image/draw-pixel
               img
               (next-c x0 x1 t)
               (next-c y0 y1 t)
               color))
            img
            (range 0 1 0.01))))

(defn draw-line-2 [img x0 y0 x1 y1 color]
  (let [xdiv (if (= x0 x1) 1 (- x1 x0))]
   (reduce (fn [g x]
             (let [t (/ (- x x0) xdiv)
                   y (+ (* y0 (- 1 t)) (* y1 t))]
               (image/draw-pixel img x y color)))
           img
           (range x0 (inc x1)))))

(defn draw-line-3 [img x0 y0 x1 y1 color]
  (let [steep (< (Math/abs (- x0 x1)) (Math/abs (- y0 y1)))
        [x0 y0 x1 y1] (if steep [y0 x0 y1 x1] [x0 y0 x1 y1])
        [x0 y0 x1 y1] (if (> x0 x1) [x1 y1 x0 y0] [x0 y0 x1 y1])
        xdiv (if (= x0 x1) 1 (- x1 x0))]
    (reduce (fn [img x]
              (let [t (/ (- x x0) xdiv)
                    y (+ (* y0 (- 1 t)) (* y1 t))
                    [x y] (if steep [y x] [x y])]
                (image/draw-pixel img x y color)))
            img
            (range x0 (inc x1)))))

(defn draw-line-4 [img x0 y0 x1 y1 color]
  (let [steep (< (Math/abs (- x0 x1)) (Math/abs (- y0 y1)))
        [x0 y0 x1 y1] (if steep [y0 x0 y1 x1] [x0 y0 x1 y1])
        [x0 y0 x1 y1] (if (> x0 x1) [x1 y1 x0 y0] [x0 y0 x1 y1])
        dx (- x1 x0)
        dy (- y1 y0)
        xdiv (if (= dx 0) 1 dx)
        derr (Math/abs (double (/ dy xdiv)))
        res (reduce (fn [acc x]
                      (let [yc (:y acc)
                            [x y] (if steep [yc x] [x yc])
                            err (+ (:err acc) derr)
                            [yn err] (if (> err 0.5)
                                       [(+ yc (if (> y1 y0) 1 -1)) (- err 1)]
                                       [yc err])]
                        {:err err
                         :y yn
                         :img (image/draw-pixel (:img acc) x y color)}))
                    {:err 0 :y y0 :img img}
                    (range x0 (inc x1)))]
    (:img res)))

(defn draw-line-5 [img x0 y0 x1 y1 color]
  (let [steep (< (Math/abs (- x0 x1)) (Math/abs (- y0 y1)))
        [x0 y0 x1 y1] (if steep [y0 x0 y1 x1] [x0 y0 x1 y1])
        [x0 y0 x1 y1] (if (> x0 x1) [x1 y1 x0 y0] [x0 y0 x1 y1])
        dx (- x1 x0)
        dy (- y1 y0)
        derr2 (* (Math/abs dy) 2)
        res (reduce (fn [acc x]
                      (let [yc (:y acc)
                            [x y] (if steep [yc x] [x yc])
                            err2 (+ (:err2 acc) derr2)
                            [yn err2] (if (> err2 dx)
                                        [(+ yc (if (> y1 y0) 1 -1)) (- err2 (* dx 2))]
                                        [yc err2])]
                        {:err2 err2
                         :y yn
                         :img (image/draw-pixel (:img acc) x y color)}))
                    {:err2 0 :y y0 :img img}
                    (range x0 (inc x1)))]
    (:img res)))

(defn draw-line [img x0 y0 x1 y1 color]
  (draw-line-5 img x0 y0 x1 y1 color))

(defn draw-lines [img lines color]
  (reduce (fn [img [v0 v1]]
            (let [[x0 y0 _] v0
                  [x1 y1 _] v1]
              (draw-line img x0 y0 x1 y1 color)))
          img
          lines))

