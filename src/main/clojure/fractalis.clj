(ns fractalis
  (:use [fractalis.turtle]
	[fractalis.lsystem])
  (:import [javax.swing JFrame JLabel]
	   [java.awt Canvas Color GradientPaint Graphics2D RenderingHints]))

(defn create-mutable-frame []
  "Creates a JFrame and returns a vector of the frame, the set-update-fn and the repaint-fn."
  (let [frame (JFrame. "Fractalis")
	update-fn (atom nil)
	c (proxy [Canvas] []
	    (paint [g]
		   (if @update-fn (@update-fn g))))
	repaint-fn (fn [] (.repaint c))]
    [(doto frame
       (.setSize 400 400)
       (.add c)
       (.setVisible true))
     #(swap! update-fn (fn [f] %))
     repaint-fn]))

(defn create-simple-frame
  "Creates a JFrame with the given update-fn and bg-fn."
  ([update-fn]
     (let [gp (GradientPaint. 0 0 Color/white 500 1500 Color/yellow)]
       (create-simple-frame update-fn (fn [g w h]
					(.setPaint g gp)
					(.fillRect g 0 0 w h)
					(.setColor g Color/black)))))
  ([update-fn bg-fn]
     (let [rh (RenderingHints. RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
	   frame (JFrame. "Fractalis")
	   c (proxy [Canvas] nil
	       (paint [g]
		      (.setRenderingHints g rh)
		      (bg-fn g (proxy-super getWidth) (proxy-super getHeight))
		      (update-fn g)))]
       (doto frame
	 (.setSize 500 500)
	 (.add c)
	 (.setVisible true)))))


(create-simple-frame
 (fn [g]
   (exec-commands
    (struct state 200 200 0 nil)
    (list
     remember
     remember
     (partial forward g 100)
     recall
     (partial rotate-deg 45)
     (partial forward g 100)
     recall
     (partial rotate-deg 90)
     (partial forward g 100)))))
