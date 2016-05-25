

(defn findTarg [targetValue llist]
  (if (zero? (count llist))
    nil
    (loop [cnt 0 ll llist xx (first ll)]
      (if (= xx targetValue)
        cnt
        (if (zero? (count ll))
          nil
          (let [rl (rest ll)]
            (recur (inc cnt) rl (first rl))))))))

(defn twoInts [ target llist ]
  (cond (empty? llist) nil
        (= (count llist) 1) (let [vv (first llist)]
                              (if (= (* 2 vv) target) (list vv vv)))
        :else (loop [cache #{} vv (first llist) ll (rest llist)]
                (let [cc (- target vv)]
                  (if (contains? cache cc)
                    (list vv cc)
                    (if (empty? ll)
                      nil
                      (recur (conj cache vv) (first ll) (rest ll))))))))
    
