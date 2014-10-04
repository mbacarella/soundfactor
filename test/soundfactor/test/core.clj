(ns soundfactor.test.core
  (:use [soundfactor.core])
  (:use [soundfactor.util])
  (:use [clojure.test]))

(defn chop4 [x]
  "Chop to 4 decimal places"
  (/ (Math/floor (* x 10000)) 10000))

(deftest test-utils
  (is (= (chop4 (degrees-to-radians 0 )) 0.0))
  (is (= (chop4 (degrees-to-radians 360)) 6.2831))
  (is (= (chop4 (degrees-to-radians 200)) 3.4906)))
