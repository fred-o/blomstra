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
	s (substitute start rules n)]
    (fn [g]
      (exec-commands
       (struct state 50 480 0 nil)
       (map { :a (partial forward g 20)
	     :b (partial forward g 20)
	     :+ (partial rotate-deg -60)
	     :- (partial rotate-deg 60) } s)))))
