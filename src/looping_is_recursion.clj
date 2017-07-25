(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))
                   ))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (when-let [[x & xs] (seq a-seq)]
    (if xs (recur xs) x)))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (empty? seq1)                     false
        (empty? seq2)                     false
        (= (first seq1) (first seq2))     (recur (rest seq1) (rest seq2))
        :else                             false
        ))

(defn find-first-index [pred a-seq]
  (loop [ix 0, a-seq (seq a-seq)]
    (when a-seq
      (if (pred (first a-seq))
        ix
        (recur (inc ix) (next a-seq))
        ))))

(defn avg [a-seq]
  (loop [sum 0, n 0, a-seq a-seq]
    (cond
      (empty? a-seq) (if (> n 0) (/ sum n) 0)
      :else          (recur (+ sum (first a-seq)) (inc n) (rest a-seq))
      )))

(defn parity [a-seq]
  (loop [odd-set #{}, a-seq a-seq]
    (cond (empty? a-seq) odd-set
          (contains? odd-set (first a-seq)) (recur (disj odd-set (first a-seq)) (rest a-seq))
          :else                             (recur (conj odd-set (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (loop [a 0, b 1, n n]
    (if (zero? n)
      a
      (recur b (+ a b) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}, result [], a-seq a-seq]
    (cond (empty? a-seq)                 result
          (contains? seen (first a-seq)) result
          :else                          (recur (conj seen (first a-seq))
                                                (conj result (first a-seq))
                                                (rest a-seq)))))

