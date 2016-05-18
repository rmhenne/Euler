;;2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;;
;;What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(require 'Factors)


(defn DivTimes [nn ii]
  "returns the number of times that ii divides nn"
  (loop [cnt 0 left nn]
    (if (or (zero? left) (= left 1) (not (zero? (mod left ii))))
      cnt
      (recur (inc cnt) (/ left ii)))))

(doseq [pp (Factors/PrimesUptoN 20)]
  (let [xlist (map (fn [xx] (DivTimes xx pp) (range 2 21)))]
    (println pp)))





