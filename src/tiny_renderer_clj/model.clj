(ns tiny-renderer-clj.model
  (:require [tiny-renderer-clj.image :as i]
            [tiny-renderer-clj.triangle :as t]
            [tiny-renderer-clj.vector :as v]
            [tiny-renderer-clj.color :as c]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.awt Color)))

(defn parse-face [s]
  (->> (str/split s #" ")
       (map #(str/split % #"/"))
       (map #(into [] (map read-string %)))
       vec))

(defn parse-vertice [s]
  (let [[x y z] (map read-string (str/split s #" "))]
    [x (- 0 y) z]))

(defn parse-texture [s]
  (->> (str/split s #" ")
       (map read-string)
       ((fn [[x y z]] [x (- 1 y) z]))))

(defn parse-model [lines]
  (reduce
   (fn [m l]
     (cond
       (str/starts-with? l "f ")
       (update m :fs conj (parse-face (subs l 2)))
       
       (str/starts-with? l  "v ")
       (update m :vs conj (parse-vertice (subs l 2)))
       
       (str/starts-with? l  "vt ")
       (update m :vts conj (parse-texture (subs l 4)))
       
       :else m))
   {:fs [] :vs [] :vts []}
   lines))

(defn texture-coord [[x y z] tw th]
  [(Math/round (* tw x)) (Math/round (* th y)) z])

(defn map-vertices [idxs vertices]
  (->> idxs
       (map #(nth vertices (dec %)))
       vec))

(defn load-model [file texture]
  (with-open [rdr (io/reader file)]
    (let [lines (line-seq rdr)
          {:keys [fs vs vts]} (parse-model lines)
          tw (.getWidth texture)
          th (.getHeight texture)]
      (reduce
       (fn [model face]
         (let [vvs (map-vertices (map first face) vs)
               vvts (map-vertices (map second face) vts)
               surface (->> (map vector vvs vvts)
                            (map (fn [[vc tc]]
                                   [vc (texture-coord tc tw th)])))]
           (conj model surface)))
       []
       fs))))

(defn load-texture [file]
  (i/load-image file))

(def light-dir [0 0 1])

(defn z-buffer [width height]
  (float-array (* width height) -1.0))

(defn face-vertices [face vertices]
  (map #(nth vertices (dec %)) (map first face)))

(defn scale-point [vc hwidth hheight]
  (let [[x y z] vc
        x (Math/round (float (* (inc x) hwidth)))
        y (Math/round (float (* (inc y) hheight)))]
    [x y z]))

(defn scale-vertices [vertices hw hh]
  (map #(scale-point % hw hh) vertices))

(defn normal [[v1 v2 v3]]
  (v/cross (v/subtract v3 v1) (v/subtract v2 v1)))

(defn nnormal [vertices]
  (v/normalize (normal vertices)))

(defn calculate-color [texture intensity p]
  (let [[x y] p]
    (-> (.getRGB texture x y)
        (Color.)
        (c/adjust-color intensity))))

(defn pixel-color [tcs texture intensity bc]
  (->> (map vector tcs bc)
         (map (fn [[tc d]] (v/multiply tc d)))
         (reduce v/add)
         (calculate-color texture intensity)
         (.getRGB)))

(defn render [model-file texture-file width height]
  (let [texture (load-texture texture-file)
        model (load-model model-file texture)
        hw (/ width 2)
        hh (/ height 2)
        img (i/create-image width height)
        img-data (-> (.getRaster img)
                     (.getDataBuffer)
                     (.getData))
        zbuf (z-buffer width height)]
    (do
      (doseq [face model]
        (let [vcs (map first face)
              n (nnormal vcs)
              i (float (v/dot n light-dir))]
          (if (> i 0)
            (let [vs (scale-vertices vcs hw hh)
                  tcs (map second face)
                  color-fn #(pixel-color tcs texture i %)]
              (t/draw-triangle img-data width height vs color-fn zbuf)))))
      img)))

