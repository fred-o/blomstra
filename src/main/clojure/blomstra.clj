(ns blomstra
  (:use [blomstra.turtle]
	[blomstra.ui]
	[clojure.contrib.import-static])
  (:import [java.awt Graphics2D]))

(import-static java.lang.Math pow sqrt)

;; Helper functions

(defn- get-definition [d & ks]
  "Given a definition-list and a list of keys, returns a concated list of all
the values corresponding to those keys."
  (mapcat rest
	  (let [s (set ks)]
	    (filter #(s (first %)) d))))

(defn- symkey [s]
  "Convert a symbol to a keyword."
  (keyword (str s)))

(defn- add-symbol [s v st]
  "Add a symbol to a symbol table."
  (assoc st
    (symkey s)
    (if (= (count v) 1) (first v)
      (cons 'partial v))))
  

(defn- make-symbol-table
  "Creates a map of symbols and their meanings."
  ([s] (make-symbol-table s nil))
  ([s st]
     (if (empty? s) st
	 (make-symbol-table (first s) (rest s) st)))
  ([f s st]
     (if (empty? s) (throw (IllegalArgumentException. (format "Expected another symbol after '%s'" f)))
	 (let [v (first s)]
	   (if (list? v)
	     (make-symbol-table (rest s) (add-symbol f v st))
	     (make-symbol-table s st))))))

(defn- split-rules
  "Parse a list of rule definitions and split them into a list of
vectors, each containing a single rule."
  ([r] (let [kr (map symkey r)]
	 (split-rules (rest kr) [(first kr)] nil)))
  ([r a res]
     (cond (empty? r) (conj res a)
	   (= :-> (first (rest r))) (split-rules (rest r) [(first r)] (conj res a))
	   :else (split-rules (rest r) (conj a (first r)) res))))

(defn- make-rule-table [rs]
  "Parse a list of rules and produce a map of keywords and their expansions."
  (zipmap (map first rs)
	  (map #(cons 'list (rest (rest %))) rs)))

(defn- apply-maybe [f & args]
  (if (fn? f) (apply f args) f))

(defstruct lsystem-struct
  :symbols :start :rules)

;; Interface functions

(defmacro lsystem [& defs]
  "Define an L-system."
  (let [st (make-symbol-table (get-definition defs :variables :constants))
	start (map symkey (get-definition defs :start))
	rules (make-rule-table (split-rules (get-definition defs :rules)))]
    `(struct lsystem-struct
	     ~st (list ~@start) ~rules)))

(defn evolve
  "Return the result of evolving an L-system n steps."
  ([ls n] (evolve (:start ls) (:rules ls) n))
  ([s subs n]
     (if (= n 0) s
	 (evolve
	  (mapcat #(or (% subs) (list %)) s)
	  subs (dec n)))))

(def function-list 
     (memoize (fn [ls n]
		(filter identity (map (:symbols ls) (evolve ls n))))))

(defn compile-lsystem [ls n]
  "Compile an L-system into a function that draws that figure on the screen."
  (let [fs (function-list ls n)]
    (fn [g x y angle unit]
      (exec-commands (struct turtle-state g x y angle unit nil) fs))))

(defn get-bounding-box [ls n a]
  "Calculates the bounding box for a given L-system of n permutations
  with an angle of a. The box is calculated with a unit length of
  1.0 and a starting x/y of 0."
  (let [fs (function-list ls n)]
    (loop [x1 0 y1 0 x2 0 y2 0 fs fs
	   st (struct turtle-state nil 0 0 a 1.0 nil)]
      (if (empty? fs) { :x x1, :y y1, :width (- x2 x1), :height (- y2 y1)}
		   (let [{:keys [x y] :as st2 } ((first fs) st)]
		     (recur (min x1 x) (min y1 y) (max x2 x) (max y2 y) (rest fs) st2))))))

(defn get-starting-coords [bb width height border]
  "Given a bounding box, calculate starting coords and unit length."
  (let [w (- width (* 2 border))
	h (- height (* 2 border))
	m (min (/ w (:width bb)) (/ h (:height bb)))]
    {:x (+ border (* m (- (:x bb))))
     :y (+ border (* m (- (:y bb))))
     :u m }))

(defn create-draw-fn [ls & opts]
  "Takes an L-system definition and some extra arguments that describe
  how to draw that shape onto the screen and poduces a function that
  takes two arguments: n (the number of evolutions to perform on the
  L-system) and g (a java.awt.Graphics2D object). That function can
  then be fed to the fractalis.ui/create-simple-frame function to
  display the generated shape on screen."
  (let [{:keys [angle border] :or {angle 0, border 25}} (apply hash-map opts)]
    (fn [n #^Graphics2D g]
      (let [cb (.getClipBounds g)]
	(if (not (nil? cb))
	  (let [cw (.getWidth cb)
		ch (.getHeight cb)
		a (apply-maybe angle n)
		{:keys [x y u]} (get-starting-coords (get-bounding-box ls n a) cw ch border)
		f (compile-lsystem ls n)]
	    (f g x y a u)))))))
