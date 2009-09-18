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

(defstruct lsystem-struct
  :symbols :start :rules)

;; Interface functions

(defmacro lsystem [& defs]
  "Define a L-system."
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

(defn create-draw-fn [ls n]
  "Compile an L-system into a function that draws that figure on the screen."
  (let [fns (filter identity (map (:symbols ls) (evolve ls n)))]
    (fn [g x y angle unit]
      (exec-commands (struct turtle-state g x y angle unit nil) fns))))

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
