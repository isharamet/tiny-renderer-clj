(ns tiny-renderer-clj.line
  (:import (java.awt Color)))

(defn draw-pixel [graphics x y]
  (doto graphics
    (.drawRect x y  0 0)))

(defn draw-line-1 [graphics x0 y0 x1 y1 color]
  (let [next-c (fn [s e t] (Math/floor (double (+ s (* (- e s) t)))))
        graphics (doto graphics (.setColor color))]
    (reduce (fn [g t] (draw-pixel g (next-c x0 x1 t) (next-c y0 y1 t)))
            graphics
            (range 0 1 0.01))))

(defn draw-line-2 [graphics x0 y0 x1 y1 color]
  (let [graphics (doto graphics (.setColor color))]
    (reduce (fn [g x] (let [t (/ (- x x0) (- x1 x0))
                          y (+ (* y0 (- 1 t)) (* y1 t))]
                      (draw-pixel g x y)))
          graphics
          (range x0 (inc x1)))))

(defn draw-line-3 [graphics x0 y0 x1 y1 color]
  (let [steep (< (Math/abs (- x0 x1)) (Math/abs (- y0 y1)))
        [x0 y0 x1 y1] (if steep [y0 x0 y1 x1] [x0 y0 x1 y1])
        [x0 y0 x1 y1] (if (> x0 x1) [x1 y1 x0 y0] [x0 y0 x1 y1])
        graphics (doto graphics (.setColor color))]
    (reduce (fn [g x] (let [t (/ (- x x0) (- x1 x0))
                            y (+ (* y0 (- 1 t)) (* y1 t))
                            [x y] (if steep [y x] [x y])]
                        (draw-pixel g x y)))
            graphics
            (range x0 (inc x1)))))

(defn draw-line-4 [graphics x0 y0 x1 y1 color]
  (let [steep (< (Math/abs (- x0 x1)) (Math/abs (- y0 y1)))
        [x0 y0 x1 y1] (if steep [y0 x0 y1 x1] [x0 y0 x1 y1])
        [x0 y0 x1 y1] (if (> x0 x1) [x1 y1 x0 y0] [x0 y0 x1 y1])
        dx (- x1 x0)
        dy (- y1 y0)
        derr (Math/abs (double (/ dy dx)))
        graphics (doto graphics (.setColor color))
        res (reduce (fn [acc x]
                      (let [yc (:y acc)
                            [x y] (if steep [yc x] [x yc])
                            err (+ (:err acc) derr)
                            [yn err] (if (> err 0.5)
                                       [(+ yc (if (> y1 y0) 1 -1)) (- err 1)]
                                       [yc err])]
                        {:err err
                         :y yn
                         :g (draw-pixel (:g acc) x y)}))
                    {:err 0 :y y0 :g graphics}
                    (range x0 (inc x1)))]
    (:g res)))

(defn draw-line-5 [graphics x0 y0 x1 y1 color]
  (let [steep (< (Math/abs (- x0 x1)) (Math/abs (- y0 y1)))
        [x0 y0 x1 y1] (if steep [y0 x0 y1 x1] [x0 y0 x1 y1])
        [x0 y0 x1 y1] (if (> x0 x1) [x1 y1 x0 y0] [x0 y0 x1 y1])
        dx (- x1 x0)
        dy (- y1 y0)
        derr2 (* (Math/abs dy) 2)
        graphics (doto graphics (.setColor color))
        res (reduce (fn [acc x]
                      (let [yc (:y acc)
                            [x y] (if steep [yc x] [x yc])
                            err2 (+ (:err2 acc) derr2)
                            [yn err2] (if (> err2 dx)
                                        [(+ yc (if (> y1 y0) 1 -1)) (- err2 (* dx 2))]
                                        [yc err2])]
                        {:err2 err2
                         :y yn
                         :g (draw-pixel (:g acc) x y)}))
                    {:err2 0 :y y0 :g graphics}
                    (range x0 (inc x1)))]
    (:g res)))
