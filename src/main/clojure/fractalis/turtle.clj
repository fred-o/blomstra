(ns fractalis.turtle
    (:import [java.awt Graphics2D]))

(defstruct state :x :y :angle :prev-pos)

(defn- newpos [len st]
  [(+ (:x st) (* (Math/cos (:angle st)) len))
   (+ (:y st) (* (Math/sin (:angle st)) len))])

(defn forward [g len st]
  (let [[x y] (newpos len st)]
    (.drawLine g (:x st) (:y st) x y)
    (assoc st :x x :y y)))

(defn skip [len st]
  (let [[x y] (newpos len st)]
    (assoc st :x x :y y)))

(defn rotate-deg [deg st]
  (assoc st :angle (+ (:angle st) (* (/ deg 360) 2 Math/PI))))

(defn remember [st]
  (assoc st :prev-pos st))

(defn recall [st]
  (:prev-pos st))

(defn exec-commands [st cmds]
  (if (empty? cmds) st
      (recur
       ((first cmds) st)
       (rest cmds))))


       
