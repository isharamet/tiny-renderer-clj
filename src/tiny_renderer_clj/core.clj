(ns tiny-renderer-clj.core
  (:require [tiny-renderer-clj.model :as model]
            [tiny-renderer-clj.image :as image])
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
  (time
   (init 800 800)))

;; (time (init 400 400))

;; (defn get-rgb [data i]
;;   (let [j (* i 4)
;;         a (aget ^bytes data j)
;;         r (aget ^bytes data (+ j 1))
;;         g (aget ^bytes data (+ j 2))
;;         b (aget ^bytes data (+ j 3))]
;;     (+ (bit-shift-left a 24)
;;        (bit-shift-left r 16)
;;        (bit-shift-left g 8)
;;        b)))

;; (def texture (model/load-texture texture-resource))

;; (time (model/load-model model-resource texture))


