(ns pongserv.core)

(defn rects-collide? [[x  y  width  height]
                      [x' y' width' height']]
  (if (or (< (+ x width) x')
          (> x (+ x' width'))
          (< (+ y height) y')
          (> y (+ y' height')))
    false
    true))

(defn centred-rect [x y width height]
  [(- x (/ width 2)) (- y (/ height 2)) width height])

(defn translate [[x y width height] dx dy]
  [(+ x dx) (+ y dy) width height])
