;; Generates an infinite sequence of prime numbers
;; using the Sieve of Eratosthenes

(defn sieve [lst]
  (cons (first lst)
        (filter #(not (zero? (mod % (first lst)))) (rest lst))))

(defn primes []
  (sieve (iterate inc 2)))
