(ns hamming.encode
  (:require [hamming.utils :as utils]))

(defn- minima-power
  "Takes an interger n and returns an interger m such that 2^(m) is larger or equal to n but 2^(m-1) is not.
   Said another way, 2 to the power of m is the smallest power of 2 still larger than n."
  [n]
  (loop [x 0]
    (if (< n (Math/pow 2 x))
      x
      (recur (inc x)))))

(defn- hamming-block-size
  "Takes a message and checks if it can be fit into a Hamming block based on the length.
   If the message is the proper length, the number of needed parity bits is returned.
   If the message is not a proper length nil is returned.
   See https://en.wikipedia.org/wiki/Hamming_code for more details."
  [message]
  (let [k (count message)
        r (minima-power k)]
    (when (== (-> (Math/pow 2 r)
                  (- r)
                  (- 1))
              k)
      (+ r 1))))

(defn- powers-of-two
  "Takes an integer n and returns a set of all the powers of 2 from 0 to n."
  [n]
  (->> (range n)
       (map #(->> % (Math/pow 2) int))
       (into #{})))

(defn- create-hamming-block-template
  "Takes a message and the number of needed parity bits and returns a Hamming block template.
   A template refers to a block where dummy parity bits have been inserted along side the message."
  [message p]
  (let [target-block-size (+ (count message) p)
        possible-powers (powers-of-two p)]
    (loop [idx    1
           block  [0] ;; the first bit is the meta-parity bit.
           message message]
      (cond
        ;; When the targer-block-size is reached, return the block.
        (= (count block) target-block-size)
        block

        ;; When the current index is a power of 2 insert a dummy parity bit.
        (contains? possible-powers idx)
        (recur (inc idx)
               (conj block 0)
               message)

        ;; When the current index is not a power of 2 insert the first bit in the remaining message.
        :else
        (recur (inc idx)
               (conj block (first message))
               (rest message))))))

(defn- int->binary-vec
  "Takes an interger x and a target digit length n.
   Returns a bit-vector representation of x with leading 0s needed to achieve length n.
   The first bit in the representation corresponds to the 1s place."
  [x n]
  (let [bit-vec (as-> x $
                  (Integer/toBinaryString $)
                  (clojure.string/split $ #"")
                  (map #(Integer/parseInt %) $))
        diff (- (- n 1)
                (count bit-vec))]
    (->> bit-vec
         (concat (repeat diff 0)) ;; append on needed leading 0s
         reverse)))

(defn- update-parity-bits
  "Takes a Hamming block template and bit vector of how the parity bits need to be set.
   If the bit vector is nil it means the parity bits are already properly set.
   Returns the Hamming block with its parity bits properly set."
  [target-parity-bits block-template]
  (if target-parity-bits
    (->> target-parity-bits
         utils/enumerate
         (reduce (fn [corrected-code [idx bit]]
                   (let [insert-idx (int (Math/pow 2 idx))]
                     (assoc corrected-code insert-idx bit)))
                 block-template))
    block-template))

(defn- update-meta-parity-bit
  "Takes a Hamming block template and returns it with the meta-parity bit properly set."
  [block-template]
  (let [meta-parity (mod (reduce + block-template) 2)]
    (assoc block-template 0 meta-parity)))

(defn encode
  "Takes a binary message of any length.
   When the message is 'proper size' it inserts encoding bits.
   When the message is not 'proper size' it returns an error-keyword.
   For an explaination of 'proper size' checkout https://en.wikipedia.org/wiki/Hamming_code."
  [message]
  (if-let [num-parity-bits (hamming-block-size message)]
    (let [block-template    (create-hamming-block-template message num-parity-bits)
          parity-bit-target (-> block-template
                                utils/find-parity-bit-target-settings
                                (int->binary-vec num-parity-bits))]
      (->> block-template
           ((partial update-parity-bits parity-bit-target))
           update-meta-parity-bit))
    :error/improper-message-length))
