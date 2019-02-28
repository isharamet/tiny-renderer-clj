(ns tiny-renderer-clj.triangle
  (:require [tiny-renderer-clj.color :as color]
            [tiny-renderer-clj.line :as line])
  (:import (java.awt Color)))

(defn draw-triangle-1
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

