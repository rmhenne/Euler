(ns core
  (:gen-class))

(defn ff "" [xx] (or (= (mod xx 3) 0) (= (mod xx 5) 0)))

(defn fib-lim 
  "lazy fib seq up to limit _lim_"
  [lim]
  ((fn rfib [a b]
       (if (< b lim)
	   (lazy-seq (cons a (rfib b (+ a b))))
	 (cons a nil)))
   0 1))


(defn prob1 "" []
  (reduce +
	  (filter (fn [xx] (or (= (mod xx 3) 0) (= (mod xx 5) 0)))
		  (range 1000))))

(defn prob2 "" []
  (reduce + (filter (fn [x] (= (mod x 2) 0)) (fib-lim 4000000))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "anser to prob 1 is " (prob1)))

