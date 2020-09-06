(ns hamming.decode
  (:require [hamming.utils :as utils]))

(defn- fix-error
  "Takes the idx of the error and the code containing the error.
   Returns the code with the error resolved."
  [idx code]
  (update code idx utils/flip))

(defn decode
  "All codes will be vectors of 0s and 1s to match the datastructure used in the video.
   Assumes the supplied code has been properly encoded before any noise mutated it.
   The supplied code may or may-not contain 1 or 2 bits of noise.
   When no errors are detected the original code is returned.
   When atleast one error is detected, the (partially) corrected-code is returned."
  [code]
  (if-let [idx (utils/find-error-idx code)]
    (fix-error idx code)
    code))

(defn- meta-parity-passed?
  "Takes a code and checks if the parity is even or odd.
   An odd parity signals an odd number of code errors.
   An even parity may indicate no errors if used in conjunction with the individual partiy checks."
  [code]
  (let [parity (mod (reduce + code) 2)]
    (zero? parity)))

(defn extended-decode
  "Assumes the supplied code may contain 0, 1, or 2 bits of noise.
   When no errors are detected the original code is returned.
   When exactly one error is detected, the corrected-code is returned.
   When 2 errors are detected, an error-keyword is returned."
   [code]
   (let [decoded (decode code)]
     (cond
       ;; 0 errors
       (= decoded code)
       code

       ;; 1 error
       (not (meta-parity-passed? code))
       decoded

       ;; 2 errors
       :else
       :two-errors)))
