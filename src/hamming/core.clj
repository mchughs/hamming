(ns hamming.core
  (:require [hamming.encode :refer [encode]]
            [hamming.decode :refer [extended-decode]]
            [hamming.utils :as utils]))

(defn extract-message [code]
  (let [message-indices (utils/get-message-bit-indices code)]
    (->> code
         utils/enumerate
         (filter (fn [[idx _]] (contains? message-indices idx)))
         (map second))))

(defn zap-one-bit [code]
  (let [bit-to-zap (rand-int (count code))
        error-bit (utils/flip (nth code bit-to-zap))]
    (assoc code bit-to-zap error-bit)))

(defn run-encode-decode-loop
  "If everything works as it should, the returned message should be the same as the supplied message."
  [message]
  (-> message
      encode
      zap-one-bit
      extended-decode
      extract-message))

(defn gen-n-bit-vector [n]
  (vec (take n (repeatedly #(rand-int 2)))))
