(ns tiny-renderer-clj.color
  (:import (java.awt Color)))

(defn random-color []
  (let [r (rand-int 255)
        g (rand-int 255)
        b (rand-int 255)]
    (Color. r g b)))

(defn adjust-color [color intensity]
  (let [[r g b] (.getRGBComponents color nil)
        i (float intensity)]
    (Color. (* r i) (* g i) (* b i) 1.0)))
