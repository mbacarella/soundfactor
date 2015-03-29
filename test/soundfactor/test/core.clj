(ns soundfactor.test.core
  (:use [soundfactor.core])
  (:require [soundfactor.util :as util])
  (:require [soundfactor.beat-this :as beat-this])
  (:use [clojure.test]))

(deftest test-utils
  ;; the array returned by fft is 2x the size of sig (the second half is junk)
  (let [sig (double-array 100 (range))
        fft (util/fft sig)]
    (is (= (count sig) (/ (count fft) 2))))
  ;; TEST: IFFT
  ;; TEST: should the inverse of the inverse be the original FFT?
  )

(deftest test-beat-this-filter-bank
  (let [sig    (double-array 100 (range))
        matrix (beat-this/filter-bank sig)]
    (is (= (count (:rows matrix)) beat-this/nbands))))
