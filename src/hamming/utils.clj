(ns hamming.utils)

(defn enumerate [coll]
  (map-indexed
    (fn [idx bit]
      [idx bit])
    coll))

(defn flip [bit]
  (mod (+ 1 bit) 2))

(defn extract-active-bit-indices
  "Takes a collection of (bit, index) tuples.
   If the bit is active, its index is appended to the list."
  [coll]
  (reduce
    (fn [list [idx bit]]
      (if (= 1 bit)
        (conj list idx)
        list))
    '()
    coll))

(defn find-error-idx
  "Takes in a code and xor's the indices of all the active bits in the code.
   If the result is 0, then no error has been found, and the function returns nil."
  [code]
  (->> code
       enumerate
       extract-active-bit-indices
       (reduce bit-xor)
       ((fn [n] (when-not (zero? n) n)))))

(def find-parity-bit-target-settings
  "We're aliasing the same function under two names just for pedagogical clarity.
   In the decoding context you use this function to find a single error in your code.
   In the encoding context you use this to find out what you need to set your parity bits to in order to set up a valid encoding."
  find-error-idx)

(defn- is-a-power-of-two? [n]
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

(defn get-message-bit-indices
  "Returns a set of the indices of the message bits inside a Hamming block."
  [code]
  (->> code
       count
       range
       (remove is-a-power-of-two?)
       (into #{})))
