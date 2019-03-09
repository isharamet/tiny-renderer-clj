(ns tiny-renderer-clj.core
  (:require [tiny-renderer-clj.image :as image] 
            [tiny-renderer-clj.model :as model])
  (:import (java.awt Color)
           (javax.swing JFrame JPanel))
  (:gen-class))

(defn draw [file w h]
  (let [[img _] (model/render file w h)]
    (proxy [JPanel] []
      (paintComponent [graphics]
        (proxy-super paintComponent graphics)
        (doto graphics
          (.drawImage img 0 0 nil))))))

(defn render [w h]
  (let [panel (doto (draw "resources/african_head.obj" w h)
                (.setOpaque true)
                (.setBackground Color/black))]
   (doto (JFrame. "tiny-renderer-clj")
     ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
     (.setContentPane panel)
     (.setSize w (+ h 30))
     (.setVisible true))))

(defn -main []
  (render 500 500))

(time
 (render 500 500))

