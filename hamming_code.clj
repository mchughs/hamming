(ns hamming-code
  "Clojure version of the solution described in \"Hamming codes part 2, the elegance of it all\" by \"3Blue1Brown\" at https://youtu.be/b3NxrZOu_CE")

(defn enumerate [coll]
  (map-indexed (fn [idx bit]
                 [idx bit])
    coll))

(defn flip [bit]
  (mod (+ 1 bit) 2))

(defn decode
  "All codes will be vectors of 0s and 1s to match the datastructure used in the video.
   Assumes the supplied code has been properly encoded before any noise mutated it.
   The supplied code may or may-not contain 1 or 2 bits of noise.
   When no errors are detected the original code is returned.
   When atleast one error is detected, the (partially) corrected-code is returned."
  [code]
  (letfn [(find-active-bit-indices [indices-vec [idx bit]]
            (if (= 1 bit)
              (conj indices-vec idx)
              indices-vec))

          (find-error-idx [noisy-code]
            (->> noisy-code
              enumerate
              (reduce find-active-bit-indices [])
              (reduce bit-xor)))]

    (let [idx (find-error-idx code)
          no-error-detected? (zero? idx)]

      (if no-error-detected?
        code
        (letfn [(fix-noisy-value [idx-of-error noisy-code]
                  (-> noisy-code
                      (nth idx-of-error)
                      flip))]
          (let [cleaned-value (fix-noisy-value idx code)]
            (assoc code idx cleaned-value)))))))

(defn extended-decode
  "The supplied code may or may-not contain 1 or 2 bits of noise.
   When no errors are detected the original code is returned.
   When exactly one error is detected, the corrected-code is returned.
   When 2 errors are detected, an error-keyword is returned."
   [code]
   (letfn [(meta-parity-passed? [code]
             (let [meta-parity-bit (first code)
                   meta-parity (->> code
                                    rest
                                    (reduce +)
                                    flip)]
               (= meta-parity-bit meta-parity)))]
     (let [partially-corrected-code (decode code)]
       (cond
         (= partially-corrected-code code)
         code

         (meta-parity-passed? code)
         partially-corrected-code

         :else
         :error/more-than-two-errors))))

(defn encode
  "Takes a binary message of any length.
   When the message is 'proper size' it inserts encoding bits.
   When the message is not 'proper size' it returns an error-keyword.
   For an explaination of 'proper size' checkout https://en.wikipedia.org/wiki/Hamming_code."
  [message]
  (letfn [(nearest-power-of-two [n] ;; actually returns the power of the smallest power of 2 that is larger than n
            (loop [x 0]
              (if (< n (Math/pow 2 x))
                x
                (recur (inc x)))))

          (proper-length? [message]
            (let [k (count message)
                  r (nearest-power-of-two k)]
              (when (== (-> (Math/pow 2 r) (- r) (- 1))
                        k)
                (+ r 1))))]

    (if-let [num-needed-encoding-bits (proper-length? message)]
      (letfn [(insert-dummy-bits [message r]
                (let [target-length (+ (count message) r)
                      possible-powers-of-two (->> (range r)
                                                  (map #(->> % (Math/pow 2) int))
                                                  (into #{}))]
                  (loop [start   1
                         encode  [0]
                         message message]
                    (cond
                      (= (count encode) target-length)
                      encode

                      (contains? possible-powers-of-two start)
                      (recur (inc start)
                             (conj encode 0)
                             message)

                      :else
                      (recur (inc start)
                             (conj encode (first message))
                             (rest message))))))

              (find-active-bit-indices [indices-vec [idx bit]]
                (if (= 1 bit)
                  (conj indices-vec idx)
                  indices-vec))

              (find-parity-bits [code]
                (->> code
                  enumerate
                  (reduce find-active-bit-indices [])
                  (reduce bit-xor)))

              (int->binary-vec [x n]
                (let [bit-vec (as-> x $
                                (Integer/toBinaryString $)
                                (clojure.string/split $ #"")
                                (map #(Integer/parseInt %) $))
                      diff (- (- n 1)
                              (count bit-vec))]
                  (->> bit-vec
                    (concat (repeat diff 0))
                    reverse)))

              (insert-parity-bits [dummy-code parity-bits]
                (->> parity-bits
                  enumerate
                  (reduce (fn [code [idx bit]]
                            (let [insert-idx (int (Math/pow 2 idx))]
                              (assoc code insert-idx bit)))
                          dummy-code)))

              (insert-meta-parity-bit [code]
                (let [meta-parity (as-> code $
                                    (reduce + $)
                                    (mod $ 2))]
                  (assoc code 0 meta-parity)))]

        (let [dummy-code (insert-dummy-bits message num-needed-encoding-bits)
              parity-bits (-> dummy-code
                              find-parity-bits
                              (int->binary-vec num-needed-encoding-bits))]
          (-> dummy-code
              (insert-parity-bits parity-bits)
              insert-meta-parity-bit))))))

(defn extract-message [code]
  (letfn [(is-a-power-of-two? [n]
            (or (zero? n)
                (loop [x 0]
                  (let [power-of-two (Math/pow 2 x)]
                    (cond
                      (== n power-of-two)
                      true

                      (> power-of-two n)
                      false

                      :else
                      (recur (inc x)))))))
          (get-message-bit-indices [code]
            (->> code
                 count
                 range
                 (remove is-a-power-of-two?)
                 (into #{})))]

    (let [message-indices (get-message-bit-indices code)]
      (->> code
        enumerate
        (filter (fn [[idx _]] (contains? message-indices idx)))
        (map second)))))

(defn zap-one-bit [code]
  (let [bit-to-zap (rand-int (count code))
        error-bit (flip (nth code bit-to-zap))]
    (assoc code bit-to-zap error-bit)))

(defn gen-n-bit-number [n]
  (vec (take n (repeatedly #(rand-int 2)))))

(def my-message (gen-n-bit-number 11))

;; Putting it all together
(= my-message
  (-> my-message
      encode
      zap-one-bit
      extended-decode
      extract-message))

(comment
  (def message
    "3b1b original message example before it has properly been encoded."
    [0 0 1 0 0 1 1 0 1 1 0])

  (def encoded-code
    "3b1b original message example after is has properly been encoded."
    [0 0 0 0 1 0 1 0 0 0 1 1 0 1 1 0])

  (def noisy-code
    "3b1b encoded code example with one error added at index 9."
    [0 0 0 0 1 0 1 0 0 1 1 1 0 1 1 0])

  (= encoded-code (encode message))
  (= encoded-code (extended-decode noisy-code))
  (= message (extract-message encoded-code))

  (def another-message
    "3b1b other message example before it has properly been encoded."
    [1 1 0 0 1 0 1 1 0 1 1])

  (def another-encoded-code
    "3b1b other message example which has properly been encoded."
    [1 1 0 1 0 1 0 0 1 1 0 1 1 0 1 1])

  (def another-noisy-code
    "3b1b encoded code example with two errors added at indices 5 and 9."
    [1 1 0 1 0 0 0 0 1 0 0 1 1 0 1 1])

  (= another-encoded-code (encode another-message))
  (= :error/more-than-two-errors (extended-decode another-noisy-code)))
