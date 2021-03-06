(ns tiny-renderer-clj.image
  (:require [clojure.java.io :as io])
  (:import (java.awt Color)
           (java.awt.image BufferedImage)
           (javax.imageio ImageIO)))

(defn load-image [file]
  (ImageIO/read (io/file file)))

(defn create-image [w h]
  (BufferedImage. w h BufferedImage/TYPE_INT_RGB))

(defn draw-pixel [img x y color]
  (let [w (.getWidth img)
        h (.getHeight img)]
    (if (and (< 0 x w) (< 0 y h))
      (doto img (.setRGB x y (.getRGB color)))
      img)))

