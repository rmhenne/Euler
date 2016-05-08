;;2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;;
;;What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?


(require 'Factors)



;;
;; First let's try to get 1-10
;;
(def pf10 (Factors/PrimeFactors (reduce * (range 1 11))))
