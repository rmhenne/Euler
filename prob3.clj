;; Euler prob #3
;;
;; The prime factors of 13195 are 5, 7, 13 and 29.
;;
;; What is the largest prime factor of the number 600851475143 ?


(import 'System.Math)

(require 'Factors)



(def oo 600851475143)
(def ooFactors (Factors/GetFactors oo))
(def ooPrimeFactors (filter (fn [aa] (Factors/CheckPrime aa)) ooFactors))

(println "Factors of " oo " are " ooFactors )
(println "primes are " ooPrimeFactors)
(println "biggest is " (last ooPrimeFactors))

