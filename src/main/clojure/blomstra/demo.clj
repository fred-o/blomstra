(ns blomstra.demo
  (:use [blomstra]
	[blomstra.ui]
	[blomstra.turtle]))

;; The Sierpinski triangle

(def sierpinski
     (lsystem
      (:variables a (forward)
		  b (forward))
      (:constants + (left 60)
		  - (right 60))
      (:start a)
      (:rules a -> b - a - b
	      b -> a + b + a)))

(def draw-sierpinski
     (create-draw-fn sierpinski
		     :angle #(if (even? %) 0 (rad -60))))

;; (create-simple-frame (partial draw-sierpinski 8))

;; The Hilbert curve

(def hilbert
     (lsystem
      (:variables L  R)
      (:constants F (forward)
		  + (left 90)
		  - (right 90))
      (:start L)
      (:rules L -> + R F - L F L - F R +
	      R -> - L F + R F R + F L -)))

(def draw-hilbert
     (create-draw-fn hilbert))

; (create-simple-frame (partial draw-hilbert 6))

;; The Dragon curve

(def dragon
     (lsystem
      (:variables X Y)
      (:constants F (forward)
		  + (right 90)
		  - (left 90))
      (:start F X)
      (:rules X -> X + Y F
	      Y -> F X - Y)))

(def draw-dragon
     (create-draw-fn dragon
		     :angle #(rad (* -45 (mod % 8)))))

;; (create-simple-frame (partial draw-dragon 14))

(def plant
     (lsystem
      (:variables X
		  F (forward))
      (:constants + (right 25)
		  - (left 25)
		  < (remember)
		  > (recall))
      (:start X)
      (:rules X -> F - < < X > + X > + F < + F X > - X
	      F -> F F)))

(def draw-plant
  (create-draw-fn plant :angle -90))

;(create-simple-frame (partial plant 4))