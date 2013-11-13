(ns simple-check.generators-test
  (:use clojure.test)
  (:require [simple-check.generators :as gen]
            [simple-check.clojure-test :refer (defspec)]
            [simple-check.properties :as prop]))

(defspec map-generates-maps-in-the-given-size 100
  (prop/for-all [m (gen/map gen/int gen/int 0 1)]
                (is (<= (count (keys m)) 1))))
