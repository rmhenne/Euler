;;2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;;
;;What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(require 'Factors)
(import 'System.Math)


(defn DivTimes [nn ii]
  "returns the number of times that ii divides nn"
  (loop [cnt 0 left nn]
    (if (or (zero? left) (= left 1) (not (zero? (mod left ii))))
      cnt
      (recur (inc cnt) (/ left ii)))))


(let [answer 
      (let [flist (Factors/PrimesUptoN 21)]
        (loop [ll flist
               ii (first ll)
               prod 1]
          (let [maxv (reduce max (map (fn [xx] (DivTimes xx ii)) (range 2 21)))
                restlist (rest ll)
                newprod (int (* prod (Math/Pow ii maxv)))]

            (if (zero? (count restlist))
              newprod
              (recur restlist (first restlist) newprod)))))]

  (println "**** Answer = " answer)
  
  (doseq [xx (range 2 21)]
    (assert (zero? (mod answer xx)))))

      










