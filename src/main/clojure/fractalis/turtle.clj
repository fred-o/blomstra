(ns fractalis.turtle
    (:import [java.awt Graphics2D]))
;;
;; A simple turtle graphics implementation
;;

(defstruct turtle-state :g :x :y :angle :unit :prev-pos)

(defn rad [d]
  (mod 
   (/ (* d 2 Math/PI) 360)
   (* 2 Math/PI)))

(defn- newpos [mul st]
  (let [l (* mul (:unit st))]
    [(+ (:x st) (* (Math/cos (:angle st)) l))
     (+ (:y st) (* (Math/sin (:angle st)) l))]))

(defn forward
  ([st] (forward 1 st))
  ([mul st]
     (let [[x y] (newpos mul st)]
       (.drawLine (:g st) (:x st) (:y st) x y)
       (assoc st :x x :y y))))

(defn skip [len st]
  (let [[x y] (newpos len st)]
    (assoc st :x x :y y)))

(defn rotate-deg [deg st]
  (assoc st :angle (+ (:angle st) (rad deg))))

(defn left [deg st]
  (rotate-deg (- deg) st))

(def right rotate-deg)

(defn remember [st]
  (assoc st :prev-pos st))

(defn recall [st]
  (:prev-pos st))

(defn exec-commands [st cmds]
  (if (empty? cmds) st
      (recur
       ((first cmds) st)
       (rest cmds))))


       
