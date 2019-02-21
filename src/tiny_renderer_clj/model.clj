(ns tiny-renderer-clj.model
  (:require [tiny-renderer-clj.line :as line]
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

(defn render [graphics file width height]
  (let [model (load-model file)
        {:keys [faces vertices]} model
        hh (/ height 2)
        hw (/ width 2)]
    (reduce (fn [g face]
              (let [fvs (map #(nth vertices (dec %)) face)
                    fes (partition 2 1 fvs fvs)]
                (reduce (fn [g [v0 v1]]
                          (let [[x0 y0 _] v0
                                [x1 y1 _] v1
                                x0 (* (inc x0) hw)
                                y0 (* (inc y0) hh)
                                x1 (* (inc x1) hw)
                                y1 (* (inc y1) hh)]
                            (line/draw-line g x0 y0 x1 y1 Color/white)))
                         g
                         fes)))
            graphics
            faces)))

