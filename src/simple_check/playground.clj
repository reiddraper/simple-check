(ns simple-check.playground
  (:import java.util.Random)
  (:require [simple-check.generators :as gen]
            [clj-tuple])
  (:refer-clojure :exclude [int vector list keyword
                            char boolean byte bytes]))

(defn shrink-seq
  [coll]
  (if (empty? coll)
    coll
    (let [head (first coll)
          tail (rest coll)]
      (concat [[tail (shrink-seq tail)]]
              (map (fn [x] [x (shrink-seq x)]) (for [x (gen/shrink-seq tail)] (cons head x)))
              (map (fn [x] [x (shrink-seq x)]) (for [y (gen/shrink head)] (cons y tail)))))))

(defn vector
  [generator]
  (fn [size rand-seed]
    (let [num-elements (Math/abs (long (gen/call-gen gen/int rand-seed size)))
          v (vec (repeatedly num-elements #(gen/call-gen generator rand-seed size)))]
      )))
