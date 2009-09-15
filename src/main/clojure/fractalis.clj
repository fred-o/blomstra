(ns fractalis
  (:import [javax.swing JFrame JLabel]))

(let [frame (JFrame. "Fractalis")]
  (doto frame
    (.setSize 400 400)
    (.setVisible true)))