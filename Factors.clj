
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


(defn Expand [hh kk]
;;  {pre: [(contains? hh kk)] }
  (loop [vv (let [xx (get hh kk)] (if (list? xx) xx (list xx)))
         newHash (dissoc hh kk)
         ii (first vv)]
    (if (zero? (count vv))
      newHash
      (let [rv (rest vv)]
        (recur rv (assoc hh (+ kk ii) ii) (if (zero? (count rv)) 0 (first rv)))))))
              
    
  


(defn GenPrimes []
  (println 2)
  (loop [xx 3 primeHash { } ]
    (if (not (contains? primeHash xx))
      (println xx)
      (recur (+ 2 xx) (assoc primeHash (* xx xx) xx)))))

      
      
      




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
    
