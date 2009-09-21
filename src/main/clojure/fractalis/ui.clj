(ns fractalis.ui
  (:import [javax.swing JFrame JLabel]
	   [java.awt Canvas Color GradientPaint Graphics2D RenderingHints]))

;;
;; General functions for managing graphic interfaces in Swing
;;

(defn gradient-fn
  ([c] (gradient-fn Color/white c))
  ([c1 c2]
     (let [gp (GradientPaint. 0 0 c1 500 1500 c2)]
       (fn [g]
	 (let [cb (.getClipBounds g)]
	   (.setPaint g gp)
	   (.fillRect g 0 0 (.getWidth cb) (.getHeight cb)))))))

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
     (create-simple-frame update-fn (gradient-fn Color/yellow)))
  ([update-fn bg-fn]
     (let [rh (RenderingHints. RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
	   frame (JFrame. "Fractalis")
	   c (proxy [Canvas] nil
	       (paint [g]
		      (.setRenderingHints g rh)
		      (bg-fn g)
		      (.setColor g Color/black)
		      (update-fn g)))]
       (doto frame
	 (.setSize 500 500)
	 (.add c)
	 (.setVisible true)))))
