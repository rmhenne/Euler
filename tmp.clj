

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
