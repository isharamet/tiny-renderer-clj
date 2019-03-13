(ns tiny-renderer-clj.core
  (:require [tiny-renderer-clj.model :as model])
  (:import (java.awt Color)
           (javax.swing JFrame JPanel))
  (:gen-class))

(def model-resource "resources/african_head.obj")

(def texture-resource "resources/african_head_diffuse.png")

(defn render [model-file texture-file width height]
  (let [img (model/render model-file texture-file width height)]
    (proxy [JPanel] []
      (paintComponent [graphics]
        (proxy-super paintComponent graphics)
        (doto graphics
          (.drawImage img 0 0 nil))))))

(defn init [width height]
  (let [panel (doto (render model-resource
                            texture-resource
                            width
                            height)
                (.setOpaque true)
                (.setBackground Color/black))]
    (doto (JFrame. "tiny-renderer-clj")
      ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setContentPane panel)
      (.setSize width (+ height 30))
      (.setVisible true))))

(defn -main []
  (init 500 500))

(time
 (init 500 500))

