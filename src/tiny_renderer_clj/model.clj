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

(def light-dir [0 0 1])

(defn z-buffer [width height]
  (vec (replicate (* width height) Integer/MIN_VALUE)))

(defn face-vertices [face vertices]
  (map #(nth vertices (dec %)) face))

(defn scale-point [[x y z] hwidth hheight]
  (let [x (Math/round (float (* (inc x) hwidth)))
        y (Math/round (float (* (inc y) hheight)))]
    [x y z]))

(defn scale-vertices [vertices hw hh]
  (map #(scale-point % hw hh) vertices))

(defn normal [[v1 v2 v3]]
  (v/cross (v/subtract v3 v1) (v/subtract v2 v1)))

(defn nnormal [vertices]
  (v/normalize (normal vertices)))

(defn render [file width height]
  (let [model (load-model file)
        {:keys [faces vertices]} model
        hw (/ width 2)
        hh (/ height 2)
        [img _] (reduce 
                 (fn [[img zbuf] face]
                   (let [fvs (face-vertices face vertices) 
                         n (nnormal fvs)
                         i (float (v/dot n light-dir))]
                     (if (> i 0)
                       (let [vs (scale-vertices fvs hw hh)
                             color (Color. i i i)]
                         (triangle/draw-triangle img vs color zbuf))
                       [img zbuf])))
                 [(image/create-image width height) (z-buffer width height)]
                 faces)]
    img))

