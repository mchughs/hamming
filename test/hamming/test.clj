(ns hamming.test
  (:require [clojure.test :refer :all]
            [hamming.core :as core]
            [hamming.encode :refer [encode]]
            [hamming.decode :refer [extended-decode]]
            [hamming.utils :as utils]))

(def message
  "3b1b original message example before it has properly been encoded."
  [0 0 1 0 0 1 1 0 1 1 0])

(def encoded-code
  "3b1b original message example after is has properly been encoded."
  [0 0 0 0 1 0 1 0 0 0 1 1 0 1 1 0])

(def noisy-code
  "3b1b encoded code example with one error added at index 9."
  [0 0 0 0 1 0 1 0 0 1 1 1 0 1 1 0])

(deftest original-test
  (testing "Test if over the course of encoding and decoding our message is recoverable."
    (is (not= encoded-code message))

    (is (= encoded-code (-> message encode)))

    (is (not= encoded-code (-> message encode core/zap-one-bit)))

    (is (= encoded-code (-> noisy-code extended-decode)))
    (is (= encoded-code (-> message encode core/zap-one-bit extended-decode)))

    (is (= message (-> noisy-code extended-decode core/extract-message)))
    (is (= message (-> message encode core/zap-one-bit extended-decode core/extract-message)))

    (is (= message (core/run-encode-decode-loop message)))))

(def another-message
  "3b1b other message example before it has properly been encoded."
  [1 1 0 0 1 0 1 1 0 1 1])

(def another-encoded-code
  "3b1b other message example which has properly been encoded."
  [1 1 0 1 0 1 0 0 1 1 0 1 1 0 1 1])

(def another-noisy-code
  "3b1b encoded code example with two errors added at indices 5 and 9."
  [1 1 0 1 0 0 0 0 1 0 0 1 1 0 1 1])

(deftest another-test
  (testing "Test if over the course of encoding and decoding our other message is recoverable."
    (is (not= another-encoded-code another-message))

    (is (= another-encoded-code (-> another-message encode)))

    (is (not= another-encoded-code another-noisy-code))

    (is (= :two-errors (-> another-noisy-code extended-decode)))))

(def valid-message (core/gen-n-bit-vector 11))
(def invalid-message (core/gen-n-bit-vector 12))

(deftest random-message-test
  (testing "Test if over the course of encoding and decoding our random message is recoverable."
    (is (= :error/improper-message-length (-> invalid-message encode)))

    (is (= valid-message (-> valid-message core/run-encode-decode-loop)))))
