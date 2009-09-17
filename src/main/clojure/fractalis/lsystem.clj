(ns fractalis.lsystem
  (:use [fractalis.turtle]))

(defn substitute
  "Return a list where all symbols in s have been expanded into their
substitutions in subs"
  ([s subs] (substitute s subs 1))
  ([s subs n]
     (if (= n 0) s
	 (substitute
	  (mapcat #(or (% subs) (list %)) s)
	  subs (dec n)))))

(defn sierpinski [n]
  (let [start (list :a)
	rules { :a (list :b :- :a :- :b )
	       :b (list :a :+ :b :+ :a) }
	s (substitute start rules n)
	len (/ 400 (Math/pow 2 n))]
    (fn [g]
      (exec-commands
       (struct state 50 400 (if (even? n) 0 (* 2 Math/PI (/ -60 360))) nil)
       (map { :a (partial forward g len)
	     :b (partial forward g len)
	     :+ (partial rotate-deg -60)
	     :- (partial rotate-deg 60) } s)))))
