(ns tiny-renderer-clj.core
  (:require [tiny-renderer-clj.line :as line])
  (:import (java.awt Color)
           (javax.swing JFrame JPanel))
  (:gen-class))

(defn draw []
  (proxy [JPanel] []
      (paintComponent [graphics]
        (proxy-super paintComponent graphics)
        (doto graphics
          (line/draw-line-3 13 20 80 40 Color/white)
          (line/draw-line-3 20 13 40 80 Color/red)
          (line/draw-line-3 80 40 13 20 Color/red)))))

(defn render []
  (let [panel (doto (draw)
                (.setOpaque true)
                (.setBackground Color/black))]
   (doto (JFrame. "tiny-renderer-clj")
     ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
     (.setContentPane panel)
     (.setSize 300 300)
     (.setVisible true))))

(render)

(defn -main []
  (render))

