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

(defn load-model [file]
  (with-open [rdr (io/reader file)]
    (let [lines (line-seq rdr)
          model {:faces [] :vertices [] :texture-map []}]
      (reduce (fn [m l]
                (cond
                  (str/starts-with? l "f ")
                  (update m :faces conj (parse-face (subs l 2)))

                  (str/starts-with? l  "v ")
                  (update m :vertices conj (parse-vertice (subs l 2)))

                  (str/starts-with? l  "vt ")
                  (update m :texture-map conj (parse-texture (subs l 4)))

                  :else m))
              model
              lines))))

(defn load-texture [file]
  (i/load-image file))

(def light-dir [0 0 1])

(defn z-buffer [width height]
  (vec (replicate (* width height) Integer/MIN_VALUE)))

(defn face-vertices [face vertices]
  (map #(nth vertices (dec %)) (map first face)))

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

(defn texture-point [texture [x y]]
  (let [w (.getWidth texture)
        h (.getHeight texture)]
    [(Math/round (* x w)) (Math/round (* y h))]))

(defn calculate-color [texture intensity [x y]]
  (-> (.getRGB texture x y)
      (Color.)
      (c/adjust-color intensity)))

(defn pixel-color [face texture-map texture intensity bc]
  (let [idxs (map second face)
        tvs (map #(nth texture-map (dec %)) idxs)]
    (->> (map vector tvs bc)
         (map (fn [[v d]] (v/multiply v d)))
         (reduce v/add)
         (texture-point texture)
         (calculate-color texture intensity))))

(defn render [model-file texture-file width height]
  (let [model (load-model model-file)
        texture (load-texture texture-file)
        {:keys [faces vertices texture-map]} model
        hw (/ width 2)
        hh (/ height 2)
        [img _] (reduce
                 (fn [[img zbuf] face]
                   (let [fvs (face-vertices face vertices)
                         n (nnormal fvs)
                         i (float (v/dot n light-dir))]
                     (if (> i 0)
                       (let [vs (scale-vertices fvs hw hh)
                             color-fn #(pixel-color face texture-map texture i %)]
                         (t/draw-triangle img vs color-fn zbuf))
                       [img zbuf])))
                 [(i/create-image width height) (z-buffer width height)]
                 faces)]
    img))

