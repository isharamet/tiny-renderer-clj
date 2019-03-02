(ns tiny-renderer-clj.model
  (:require [tiny-renderer-clj.line :as line]
            [tiny-renderer-clj.image :as image]
            [tiny-renderer-clj.triangle :as triangle]
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
  (let [x (Math/round (double (* (inc x) hwidth)))
        y (Math/round (double (* (inc y) hheight)))]
    [x y z]))

(defn render [file width height]
  (let [model (load-model file)
        {:keys [faces vertices]} model
        hw (/ width 2)
        hh (/ height 2)
        img (image/create-image width height)]
    (reduce (fn [img face]
              (let [vs (map #(nth vertices (dec %)) face)
                    svs (map #(scale-point % hw hh) vs)
                    es (partition 2 1 svs svs)]
                ;; (line/draw-lines img es Color/green)
                (triangle/draw-triangle img svs)
                ))
            img
            faces)))

