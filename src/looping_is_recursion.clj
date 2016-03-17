(ns looping-is-recursion)

(defn power [base exp]
  (if (zero? exp)
    1
    (let [helper (fn [acc n k]
                   (if (zero? k)
                     acc
                     (recur (* acc n) n (dec k))))]
      (helper base base (dec exp)))))

(defn last-element [a-seq]
   (let [helper (fn [some-seq]
                 (if (or (empty? some-seq) (empty? (rest some-seq)))
                   (first some-seq)
                   (recur (rest some-seq))))]
    (helper a-seq)))


(defn seq= [seq1 seq2]
  (cond
      (and (empty? seq1)(empty? seq2)) true
      (or (empty? seq1)(empty? seq2)
      (not= (first seq1)(first seq2)))false
    :else
      (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
   (loop [n 0
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) n
      :else (recur (inc n) (rest s)))))


(defn avg [a-seq]
 (/ (apply + a-seq) (count a-seq)))

(defn parity [a-seq]
  (loop [a-seq a-seq set #{}]
         (cond
           (empty? a-seq) set
           (contains? set (first a-seq))
           (recur (rest a-seq) (disj set (first a-seq)))
         :else
           (recur (rest a-seq) (conj set (first a-seq))))))

(defn fast-fibo [n]
    (loop [n n
         fib-1 0
         fib-2  1]
    (if (< n 1)
      fib-1
      (recur (dec n) fib-2 (+ fib-1 fib-2)))))


(defn cut-at-repetition [a-seq]
 (loop [remaining a-seq acc-seq [] seen #{}]
    (if (or (empty? remaining) (contains? seen (first remaining)))
      acc-seq
      (recur (rest remaining) (conj acc-seq (first remaining)) (conj seen (first remaining))))))

