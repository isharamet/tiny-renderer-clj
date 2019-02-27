(ns tiny-renderer-clj.core
  (:require [tiny-renderer-clj.line :as line]
            [tiny-renderer-clj.model :as model]
            [tiny-renderer-clj.triangle :as triangle])
  (:import (java.awt Color)
           (javax.swing JFrame JPanel))
  (:gen-class))


(defn draw []
  (proxy [JPanel] []
      (paintComponent [graphics]
        (proxy-super paintComponent graphics)
        (doto graphics
          (model/render "resources/african_head.obj" 600 600)
          ;;(triangle/draw-filled-triangle [[10 10 0] [300 10 0] [11 300 0]] Color/white)
          ;;Vec2i(10,10), Vec2i(100, 30), Vec2i(190, 160)
          ))))

(defn render []
  (let [panel (doto (draw)
                (.setOpaque true)
                (.setBackground Color/black))]
   (doto (JFrame. "tiny-renderer-clj")
     ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
     (.setContentPane panel)
     (.setSize 600 600)
     (.setVisible true))))

(defn -main []
  (render))

(render)


