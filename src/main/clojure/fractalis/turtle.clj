(ns fractalis.turtle
    (:import [java.awt Graphics2D]))

(defstruct state :x :y :angle :prev-pos)

(defn- newpos [st len]
  [(+ (:x st) (* (Math/cos (:angle st)) len))
   (+ (:y st) (* (Math/sin (:angle st)) len))])

(defn draw [g st len]
  (let [[x y] (newpos st len)]
    (.drawLine (:x st) (:y st) x y)
    (assoc st :x x :y y)))

(defn skip [st len]
  (let [[x y] (newpos st len)]
    (assoc st :x x :y y)))

(defn rotate-deg [st deg]
  (assoc st :angle (+ (:angle st) (* (/ deg 360) 2 Math/PI))))

(defn remember [st]
  (assoc st :prev-pos st))

(defn recall [st]
  (:prev-pos st))