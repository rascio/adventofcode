(ns math)

(def rad (/ 180 Math/PI))
(defn deg->rad [degrees]
  (* rad degrees))
(defn sin [degrees]
  (-> (deg->rad degrees)
      (Math/sin)
      (Math/round)
      (int)))
(defn cos [degrees]
  (-> (deg->rad degrees)
      (Math/cos)
      (Math/round)
      (int)))


(def primes-1000 (->> (range 2 1000)
                      (reduce (fn [acc n]
                                (if (some #(= 0 (mod n %)) acc)
                                  acc
                                  (conj acc n)))
                              [])
                      (apply sorted-set)))

(defn prime? [n]
  (or (primes-1000 n)
      (->> (range (last primes-1000) n 2)
           (some #(= 0 (mod n %)))
           (not))))
(defn factorize [n]
  (loop [n' n
         acc ()]
    (if (= 1 n')
      acc
      (let [p (or (primes-1000 n')
                  (->> (range 2 (inc n'))
                       (filter prime?)
                       (filter #(= 0 (mod n' %)))
                       (first)))]
        (recur (/ n' p)
               (cons p acc))))))
(defn gcd [n m]
  (loop [n' n
         m' m]
    (cond (= n' m') n'
          (> n' m') (recur (- n' m') m')
          :else (recur (- m' n') n'))))
(defn lcm [n & rest]
  (loop [n n
         [m & tail] rest]
    (if (nil? m)
      n
      (let [cgd (gcd n m)]
        (recur (/ (* n m) cgd) tail)))))

(defn pow [a n]
  (long (Math/pow a n)))