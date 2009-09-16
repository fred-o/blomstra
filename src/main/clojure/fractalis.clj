(ns fractalis
  (:use [fractalis.turtle])
  (:import [javax.swing JFrame JLabel]
	   [java.awt Canvas Color GradientPaint Graphics2D RenderingHints]))

(def *update-fn* (atom nil))

(def *frame* 
     (let [frame (JFrame. "Fractalis")
	   c (proxy [Canvas] []
	       (paint [g]
		      (if @*update-fn* (@*update-fn* g))))]
       (doto frame
	 (.setSize 400 400)
	 (.add c)
	 (.setVisible true))))

(defn set-update-fn [f]
  (swap! *update-fn* (fn [&args] f))
  (.repaint *frame*))


(let [rh (RenderingHints. RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      gp (GradientPaint. 0 0 Color/white 500 1500 Color/yellow)]
  (swap! *update-fn*
	 (fn [&args]
	   (fn [g]
	     (.setRenderingHints g rh)
	     (.setPaint g gp)
	     (.fillRect g 0 0 400 400)
	     (.setColor g Color/black)
	     (exec-commands 
	      (struct state 100 100 0 nil)
	      (list 
	       (partial forward g 100)
	       (partial rotate-deg 55)
	       (partial forward g 100)
	       (partial rotate-deg 55)
	       (partial forward g 100)
	       (partial rotate-deg 55)
	       (partial forward g 100)
	       (partial rotate-deg 55)
	       (partial forward g 100)
	       (partial rotate-deg 55)
	       (partial forward g 100)
	       (partial rotate-deg 55)))))))





