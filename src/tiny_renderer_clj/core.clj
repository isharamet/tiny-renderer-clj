(ns tiny-renderer-clj.core
  (:require [tiny-renderer-clj.model :as model]
            [tiny-renderer-clj.triangle :as triangle])
  (:import (java.awt Color)
           (javax.swing JFrame JPanel))
  (:gen-class))

(defn render [file w h]
  (let [img (model/render file w h)]
    (proxy [JPanel] []
      (paintComponent [graphics]
        (proxy-super paintComponent graphics)
        (doto graphics
          (.drawImage img 0 0 nil))))))

(defn init [w h]
  (let [panel (doto (render "resources/african_head.obj" w h)
                (.setOpaque true)
                (.setBackground Color/black))]
   (doto (JFrame. "tiny-renderer-clj")
     ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
     (.setContentPane panel)
     (.setSize w (+ h 30))
     (.setVisible true))))

(defn -main []
  (init 500 500))

(time
 (init 500 500))

