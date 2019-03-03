(ns tiny-renderer-clj.model
  (:require [tiny-renderer-clj.line :as line]
            [tiny-renderer-clj.image :as image]
            [tiny-renderer-clj.triangle :as triangle]
            [tiny-renderer-clj.vector :as v]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.awt Color)))

(defn parse-face [s]
  (let [[_ x y z] (str/split s #" ")]
    (map #(-> %
              (str/split #"/")
              first
              read-string)
         [x y z])))

(defn parse-vertice [s]
  (let [[_ x y z] (str/split s #" ")
        [x y z] (map read-string [x y z])]
    [x (- 0 y) z]))

(defn load-model [file]
  (with-open [rdr (io/reader file)]
    (let [lines (line-seq rdr)
          model {:faces [] :vertices []}]
      (reduce (fn [m l]
                (cond
                  (str/starts-with? l "f ")
                  (update m :faces conj (parse-face l))

                  (str/starts-with? l  "v ")
                  (update m :vertices conj (parse-vertice l))

                  :else m))
              model
              lines))))

(defn scale-point [[x y z] hwidth hheight]
  (let [x (Math/round (float (* (inc x) hwidth)))
        y (Math/round (float (* (inc y) hheight)))]
    [x y z]))

(def light-dir [0 0 1])

(defn render-1 [file width height]
  (let [model (load-model file)
         {:keys [faces vertices]} model
         hw (/ width 2)
         hh (/ height 2)
         img (image/create-image width height)]
     (reduce (fn [img face]
               (let [vs (map #(nth vertices (dec %)) face)
                     svs (map #(scale-point % hw hh) vs)
                     es (partition 2 1 svs svs)
                     [v1 v2 v3] vs
                     n (v/cross (v/subtract v3 v1) (v/subtract v2 v1))
                     nn (v/normalize n)
                     intensity (float (v/dot nn light-dir))]
                 ;; Mesh rendering
                 ;; (line/draw-lines img es Color/green)
                 (do
                   ;; (println vs svs)
                   (if (> intensity 0)
                     (let [color (Color. intensity intensity intensity)]
                       (triangle/draw-triangle img svs color))
                     img))))
             img
             faces)))

(defn fast-render [graphics file width height]
  (let [model (load-model file)
        {:keys [faces vertices]} model
        hw (/ width 2)
        hh (/ height 2)]
    (reduce (fn [g face]
              (let [vs (map #(nth vertices (dec %)) face)
                    svs (map #(scale-point % hw hh) vs)
                    es (partition 2 1 svs svs)
                    [v1 v2 v3] vs
                    n (v/cross (v/subtract v3 v1) (v/subtract v2 v1))
                    nn (v/normalize n)
                    intensity (float (v/dot nn light-dir))]
                (do
                  (if (> intensity 0)
                    (let [color (Color. intensity intensity intensity)]
                      (triangle/fast-draw-triangle g svs color))
                    g))))
            graphics
            faces)))
