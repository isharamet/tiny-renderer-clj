(ns tiny-renderer-clj.model
  (:require [tiny-renderer-clj.image :as i]
            [tiny-renderer-clj.triangle :as t]
            [tiny-renderer-clj.vector :as v]
            [tiny-renderer-clj.color :as c]
            [tiny-renderer-clj.matrix :as m]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.awt Color)))

(def depth 255)

(def camera [0 0 3])

(def light-dir [0 0 1])

(defn viewport [x y w h]
  (let [vp (m/identity 4)
        hw (/ w 2.0)
        hh (/ h 2.0)
        hd (/ depth 2.0)
        tfs {[0 3] (+ x hw) 
             [1 3] (+ y hh)
             [2 3] hd
             [0 0] hw
             [1 1] hh
             [2 2] hd}]
    (reduce (fn [acc [k v]] (assoc-in acc k v)) vp tfs)))

(defn parse-face [s]
  (->> (str/split s #" ")
       (map #(str/split % #"/"))
       (map #(into [] (map read-string %)))))

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
       (map #(nth vertices (dec %)))))

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

(defn z-buffer [width height]
  (float-array (* width height) -1.0))

(defn round-point [vc hwidth hheight]
  (let [[x y z] vc
        x (Math/round (float x))
        y (Math/round (float y))
        z (Math/round (float z))]
    [x y z]))

(defn round-vertices [vertices hw hh]
  (map #(round-point % hw hh) vertices))

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

(def load-model-memo (memoize load-model))

(def load-texture-memo (memoize load-texture))

(defn render [model-file texture-file width height]
  (let [texture (load-texture-memo texture-file)
        model (load-model-memo model-file texture)
        hw (/ width 2)
        hh (/ height 2)
        img (i/create-image width height)
        img-data (-> (.getRaster img)
                     (.getDataBuffer)
                     (.getData))
        vp (viewport (/ width 8.0)
                     (/ height 8.0)
                     (* width 0.75)
                     (* height 0.75))
        pr (assoc-in (m/identity 4) [3 2] (/ -1.0 (last camera)))
        zbuf (z-buffer width height)]
    (do
      (doseq [face model]
        (let [vcs (map first face)
              n (nnormal vcs)
              i (float (v/dot n light-dir))]
          (if (> i 0)
            (let [vs (map #(m/m-to-v (reduce m/multiply [vp pr (m/v-to-m %)])) vcs)
                  rvs (scale-vertices vs hw hh)
                  tcs (map second face)
                  color-fn #(pixel-color tcs texture i %)]
              (t/draw-triangle img-data width height rvs color-fn zbuf)))))
      
      img)))

