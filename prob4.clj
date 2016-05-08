;; A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
;; Find the largest palindrome made from the product of two 3-digit numbers.


(defn IsPalindrome [nn]
  { :pre [ (string? nn) ] }
  "returns true of nn is a palindrome"
  (loop [ aa nn ]
    (let [laa (count aa)]
      (if (<= laa 1)
        true
        (if (not (= (subs aa 0 1) (subs aa (dec laa) laa)))
          false
          (recur (subs aa 1 (dec laa))))))))

(defn MapMulti [xx nn]
  (map (fn [yy] (* xx yy)) (range (inc xx) nn)))

(defn PalMulti [xx nn]
  (filter (fn [xx] (IsPalindrome (str xx))) (MapMulti xx nn)))

(defn ff []
  (loop [ii 100 acc []]
    (let [ll (PalMulti ii 999)]
      (if (>= ii 999)
        acc
        (recur (inc ii) (if (> (count ll) 0) (flatten (cons acc ll)) acc))))))

(apply max (ff))



            
