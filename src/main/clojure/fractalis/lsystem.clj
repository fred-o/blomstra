(ns fractalis.lsystem
  (:use [fractalis.turtle]))

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
  ([s st] (if (empty? s) st
	      (make-symbol-table (first s) (rest s) st)))
  ([f s st] (if (empty? s) (throw (IllegalArgumentException. (format "Expected another symbol after '%s'" f)))
		(let [v (first s)]
		  (make-symbol-table
		   (rest s) (if (list? v) (add-symbol f v st) st))))))

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

(defn compile-lsystem
     [ls n]
     "Compile an L-system into a function that draws that figure on the screen."
     (let [fs (filter identity (map (:symbols ls) (evolve ls n)))]
       (fn [g x y angle unit]
	 (exec-commands (struct turtle-state g x y angle unit nil) fs))))

(defn create-draw-fn [ls & opts]
  "Takes an L-system definition and some extra arguments that describe
how to draw that shape onto the screen and poduces a function that
takes two arguments: n (the number of evolutions to perform on the
L-system) and g (a java.awt.Graphics2D object). That function can then
be fed to the fractalis.ui/create-simple-frame function to display the
generated shape on screen."
  (let [{:keys [angle x y unit border] :or {angle 0, x 0, y 0, unit 10, border 25}} (apply hash-map opts)]
    (fn [n g]
      (let [cb (.getClipBounds g)]
	(if (not (nil? cb))
	  (let [cw (.getWidth cb)
		ch (.getHeight cb)
		a (apply-maybe angle n)
		u (* (apply-maybe unit n) (- (Math/min cw ch) 50))
		x1 (/ (* x cw) 100)
		y1 (/ (* y ch) 100)
		f (compile-lsystem ls n)]
	    (f g x1 y1 a u)))))))
	    
	    
	
    

