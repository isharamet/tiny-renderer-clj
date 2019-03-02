(ns tiny-renderer-clj.core
  (:require [tiny-renderer-clj.model :as model])
  (:import (java.awt Color)
           (javax.swing JFrame JPanel))
  (:gen-class))

(defn draw [file w h]
  (let [
        ;; image (model/render file w h)
        ]
    (proxy [JPanel] []
      (paintComponent [graphics]
        (proxy-super paintComponent graphics)
        (doto graphics
          ;; (.drawImage image 0 0 nil)
          (model/fast-render file w h)
          )))))

(defn render [w h]
  (let [panel (doto (draw "resources/african_head.obj" w h)
                (.setOpaque true)
                (.setBackground Color/black))]
   (doto (JFrame. "tiny-renderer-clj")
     ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
     (.setContentPane panel)
     (.setSize w h)
     (.setVisible true))))

(defn -main []
  (render 800 800))

(time
 (render 800 800))

