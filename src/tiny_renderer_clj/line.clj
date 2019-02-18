(ns tiny-renderer-clj.line
  (:import (java.awt Color)))

(defn draw-pixel [graphics x y]
  (doto graphics
    (.drawRect x y  0 0)))

(defn draw-pixels
  ([graphics pixels] (reduce (fn [g p] (draw-pixel g (:x p) (:y p)))
                             graphics
                             pixels))
  ([graphics pixels color] (doto graphics
                             (.setColor color)
                             (draw-pixels pixels))))

(defn draw-line-1 [graphics x0 y0 x1 y1 color]
  (let [axis-coord (fn [a b t] (Math/floor (double (+ a (* (- b a) t)))))
        line-pixels (map (fn [t] {:x (axis-coord x0 x1 t)
                                  :y (axis-coord y0 y1 t)})
                         (range 0 1 0.01))]
    (draw-pixels graphics line-pixels color)))

(defn draw-line-2 [graphics x0 y0 x1 y1 color]
  (let [line-pixels (map (fn [x]
                           (let [t (/ (- x x0) (- x1 x0))
                                 y (+ (* y0 (- 1 t)) (* y1 t))]
                             {:x x :y y})) 
                         (range x0 (inc x1)))]
    (draw-pixels graphics line-pixels color)))

(defn draw-line-3 [graphics x0 y0 x1 y1 color]
  (let [is-steep (< (Math/abs (- x0 x1)) (Math/abs (- y0 y1)))
        [x0 y0 x1 y1] (if is-steep [y0 x0 y1 x1]
                                   [x0 y0 x1 y1])
        [x0 y0 x1 y1] (if (> x0 x1) [x1 y1 x0 y0]
                                    [x0 y0 x1 y1])
        line-pixels (map (fn [x]
                           (let [t (/ (- x x0) (- x1 x0))
                                 y (+ (* y0 (- 1 t)) (* y1 t))]
                             (if is-steep {:x y :y x}
                                          {:x x :y y}))) 
                         (range x0 (inc x1)))]
    (draw-pixels graphics line-pixels color)))
