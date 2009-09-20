(ns fractalis
  (:use [fractalis.turtle]
	[fractalis.lsystem]
	[fractalis.ui]))

;; System definitions

(def sierpinski
     (lsystem
      (:variables a (forward)
		  b (forward))
      (:constants + (left 60)
		  - (right 60))
      (:start a)
      (:rules a -> b - a - b
	      b -> a + b + a)))

(def hilbert
     (lsystem
      (:variables L  R)
      (:constants F (forward)
		  + (left 90)
		  - (right 90))
      (:start L)
      (:rules L -> + R F - L F L - F R +
	      R -> - L F + R F R + F L -)))

(def dragon
     (lsystem
      (:variables X Y)
      (:constants F (forward)
		  + (right 90)
		  - (left 90))
      (:start F X)
      (:rules X -> X + Y F
	      Y -> F X - Y)))


(defn draw-sierpinski [n]
  (let [draw-fn (create-draw-fn sierpinski n)]
    (create-simple-frame
     (fn [g] (draw-fn g 50 450
                     (if (even? n) 0 (rad -60))
                     (/ 400 (Math/pow 2 n)))))))

(defn draw-hilbert [n]
  (let [draw-fn (create-draw-fn hilbert n)]
    (create-simple-frame
     (fn [g] (draw-fn g 50 450
                     0
                     (/ 400 (- (Math/pow 2 n) 1)))))))

(defn draw-dragon [n]
  (let [draw-fn (create-draw-fn dragon n)]
    (create-simple-frame
     (fn [g] (draw-fn g 400 150
                     (rad (* -45 (mod n 4)))
                     (/ 400 (Math/pow 1.5 n)))))))
