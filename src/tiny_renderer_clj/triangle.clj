(ns tiny-renderer-clj.triangle
  (:require [tiny-renderer-clj.color :as color]
            [tiny-renderer-clj.line :as line])
  (:import (java.awt Color)))

(defn draw-triangle [graphics edges]
  (reduce (fn [g [v0 v1]]
            (let [[x0 y0 _] v0
                  [x1 y1 _] v1]
              (line/draw-line g x0 y0 x1 y1 Color/white)))
          graphics
          edges))

(defn draw-filled-triangle
  ([graphics vertices]
   (let [sorted-vertices (sort-by second vertices)
         [x0 y0 _] (first sorted-vertices)
         [x1 y1 _] (second sorted-vertices)
         [x2 y2 _] (last sorted-vertices)
         height (- y2 y0)]
     (reduce (fn [g y]
               (let [second-half (> y y1)
                     segment-height (if second-half (- y2 y1) (- y1 y0))
                     ydiff (if second-half (- y2 y) (- y y0))
                     alpha (double (/ ydiff height))
                     beta (double (/ ydiff segment-height))
                     [x0 x1 x2] (if second-half [x2 x1 x0] [x0 x1 x2])
                     ax (+ x0 (Math/round (* alpha (- x2 x0))))
                     bx (+ x0 (Math/round (* beta (- x1 x0))))]
                 (line/draw-line g ax y bx y)))
             graphics
             (range y0 y2))))
  ([graphics edges color]
   (let [graphics (doto graphics (.setColor color))]
     (draw-filled-triangle graphics edges))))

(defn draw-random-colored-filled-triangle [graphics vertices]
  (draw-filled-triangle graphics vertices (color/random-color)))


