
(import 'System.Math)

(ns Factors)
  

(defn TestIt [expression]
  (if expression "Passed" "Failed"))

(defn PrintTestIt [text expression]
  (println "Running " text " : " (TestIt expression))
  expression)

(defn DivisibleBy [nn ii]
  { :pre [ (number? nn)
          (number? ii)] } 
  ;;(println "Mod " nn ii)
  (zero? (mod nn ii)))


(def SeedPrimes (sorted-set 2 3 5 7 11 13 17 19))

(defn CheckPrimeCore [seedPrimes nn]
  {:pre [(> (count seedPrimes) 0)
         (number? nn)
         (> nn 0)]}

  (if (= nn 1)
    false
    (let [divLim (Math/Ceiling (Math/Sqrt nn))
          lesserPrimes (filter (fn [yy] (< yy nn)) seedPrimes)
          factorSoFar (if (or (zero? (count lesserPrimes)) (nil? lesserPrimes)) false
                          (reduce (fn [aa bb] (or aa bb))
                                  (map (fn [xx] (DivisibleBy nn xx))
                                       lesserPrimes)))]
      
      (if factorSoFar false
          (let [lsp (last seedPrimes)]
            (loop [ii (if (= lsp 2) 3 (+ lsp 2))]
              (if (> ii divLim) true
                  (if (DivisibleBy nn ii)
                    false
                    (recur (+ ii 2))))))))))


(defn CheckPrime [nn]
  (CheckPrimeCore SeedPrimes nn))

    
(defn ExtendPrimes [seedPrimes limit]
  "Takes a sorted set of primes (seedPrimes) and augments that set upto the greates prime <= limit"
  (let [sp (if (empty? seedPrimes) (sorted-set 2) seedPrimes)
        lsp (last seedPrimes)
        ii (if (= lsp 2) 3 (+ lsp 2))]
    (loop [jj ii primes sp]
      (if (> jj limit)
        primes
        (let [isPrime (CheckPrimeCore primes jj)]
          (recur (+ jj 2) (if isPrime (conj primes jj) primes)))))))

(defn PrimesUptoN [limit]
  (ExtendPrimes SeedPrimes limit))


(defn GetFactors [nn]
  "Returns a sorted set of all of the factors of nn between 1 and nn exclusive"
  (let [divLim (Math/Ceiling (Math/Sqrt nn))]
    (loop [ii 2 factors (sorted-set)]
      (let [ff (if (DivisibleBy nn ii) ii nil)
            gg (if (not (nil? ff)) (/ nn ff) nil)]
        (if (> ii divLim) factors
            (recur (inc ii) (if (nil? ff)
                              factors
                              (if (= ff gg) (conj factors ff) (conj factors ff gg)))))))))

(defn PrimeFactors [nn]
  (let [factors (GetFactors nn)
        divLim (Math/Ceiling (Math/Sqrt nn))
        primes (ExtendPrimes (sorted-set 2 3 5 7 11) divLim)]

    ;; (println "Factors = " factors "divLim " divLim " primes " primes)
    (filter (fn [xx] (CheckPrimeCore primes xx)) factors)))


(defn HowManyTimes [nn factor]
  (loop [ii nn cnt 0]
    (if (not (zero? (mod ii factor)))
      cnt
      (recur (/ ii factor) (inc cnt)))))

(defn PrimeFactorization [nn]
  "Return Prime Factorization of NN in a hash table"
  (let [pf (PrimeFactors nn) acc {} ]

    ;;(println "pf = " pf)
    (map (fn [xx] (list xx (HowManyTimes nn xx))) pf)))


;;
;;
;; This set of fucntions implements a generative Sieve of Eratosthenes algorithm.  In this implementation, we will simply skip multiples of 2
;;
;; A hash table is used to hold lists of multiples of primes
;;
;; For instance at some point 3 will be identified as a prime.  We need to "mark off" all multiples of 3.  Turns out we only need to watch out for 3^2.  So we will insert { 9: 3 } into the hash.
;; if we need to check 9 for primality, we look up in the hash, see 9, reject 9 as a prime and insert { 12:3 } into the hash because 12 is the next multiple of 3.
;;
;;

(defn Insert [nonPrimeHash kk vv]
  "Insert a multiple (kk) prime (vv) pair into the hash."
  (if (contains? nonPrimeHash kk)
    (assoc nonPrimeHash kk (conj (get nonPrimeHash kk) vv))  ;; Another prime factor of kk. Map kk -> (vv previousFactors)
    (assoc nonPrimeHash kk (list vv)) ;; first prime factor of kk. Map kk -> (vv)
    )
  )


(defn Expand [nonPrimeHash kk]
  { :pre [(contains? nonPrimeHash kk)              ;; If we are going to expand it, it better be in the hash
          (not (empty? (get nonPrimeHash kk)))     ;; There better be at least 1 prime factor in the list
          ] }

  ;;
  ;; Get prime facts for kk, should be a list even if there is only 1.
  ;; Generate a new hash removing key kk since we have tested it for primality and we are increasing args so we will never see kk again
  ;; ii gets the first prime factor in the list
  ;;
  (loop [vv (get nonPrimeHash kk)
         newHash (dissoc nonPrimeHash kk)
         ii (first vv)]

    (let [rv (rest vv)
          nh (Insert newHash (+ kk ii) ii)]
      
      (if (empty? rv)
        nh  ;; no more prime factors for kk so return the new hash with kk "Expanded"
        (do
          (recur rv nh (first rv))
          )))))

              
    
;;
;; Takes a candidate prime and a hashTable holding the relevant non-primes.
;; Return the next prime >= firstCandidate and a new hash loaded with relevant non-primes.
;;


(defn GenPrimeStep [ firstCandidate nonPrimeHash ]
  (loop [xx firstCandidate nph nonPrimeHash]
    (if (not (contains? nph xx))
      (lazy-seq (cons xx (GenPrimeStep (inc xx) (Insert nph (* xx xx) xx))))
      (recur (inc xx) (Expand nph xx))
      )
    )
  )
  

(defn GenPrimes [countMax]
  (take countMax (GenPrimeStep 2 {})))


(defn RunTests []
  (let [retVal true]
  
    (if true
      (do
        (PrintTestIt "primality of 841" (= (CheckPrime 841) false))
        (PrintTestIt "primality of 28" (= (CheckPrime 28) false))
        (PrintTestIt "primality of 29" (= (CheckPrime 29) true))
        (PrintTestIt "primality of 839" (= (CheckPrime 839) true))
        (PrintTestIt "primality of 6857" (= (CheckPrime 6857) true))
        (PrintTestIt "primality of 6859" (= (CheckPrime 6859) false))
        (PrintTestIt "primality of 6861" (= (CheckPrime 6861) false))
        (PrintTestIt "primality of 1234169" (= (CheckPrime 1234169) false))
        (PrintTestIt "primality of 1471" (= (CheckPrime 1471) true))
        (PrintTestIt "primality of 8462696833" (= (CheckPrime 8462696833) false))
        ))
    )
  )
    
