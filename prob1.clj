;;
;; terse and boring way
;;
(println "using method 1"
         (reduce + (filter (fn [xx] (or (= (mod xx 3) 0) (= (mod xx 5) 0))) (range 3 1000))) )


;;
;; Longer and more fun way
;;

(defn levelUp
  "Advances generator (ss cur) until value returned is > last"
  [cur last ss]
  (if (> cur last)
    cur
    (loop [vv (ss cur)]
      (if (> vv last)
        vv
        (recur (ss vv))))))

(defn genSeq
  [limit step1 step2]
  (let [gen1 #(+ step1 %) gen2 #(+ step2 %)]
    ((fn rgen [last aa bb]
      (let [vv (if (< aa bb) aa bb)]
         (if (<= vv limit)
           (lazy-seq (cons vv (rgen vv (levelUp aa vv gen1)
                                    (levelUp bb vv gen2))))))) 0 step1 step2)))


(println "using method 2" 
         (reduce + (genSeq 999 3 5)) )




