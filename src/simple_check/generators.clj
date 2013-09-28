(ns simple-check.generators
  (:import java.util.Random)
  (:require [clj-tuple])
  (:refer-clojure :exclude [int vector list map keyword
                            char boolean byte bytes]))


;; RoseTree
;; ---------------------------------------------------------------------------

(defn join-rose
  [[[inner-root inner-children] children]]
  [inner-root (clojure.core/map join-rose (concat children inner-children))])

(defn rose-root
  [[root _children]]
  root)

(defn rose-children
  [[_root children]]
  children)

(defn rose-pure
  [x]
  [x []])

(defn rose-fmap
  [f [root children]]
  [(f root) (clojure.core/map (partial rose-fmap f) children)])

(defn rose-bind
  [m k]
  (join-rose (rose-fmap k m)))

;; Gen
;; ---------------------------------------------------------------------------

(defn make-gen
  ([generator-fn]
  {:gen generator-fn}))

(defn gen-pure
  [value]
  (make-gen
    (fn [rnd size]
      value)))

(defn gen-fmap
  [k {h :gen}]
  (make-gen
    (fn [rnd size]
          (k (h rnd size)))))

(defn gen-bind
  [{h :gen} k]
  (make-gen
    (fn [rnd size]
      (let [inner (h rnd size)
            {result :gen} (k inner)]
        (result rnd size)))))

(defn fmap
  [f gen]
  (gen-fmap (partial rose-fmap f) gen))

;; Helpers
;; ---------------------------------------------------------------------------

(defn random
  ([] (Random.))
  ([seed] (Random. seed)))

(defn call-gen
  [{generator-fn :gen} rnd size]
  (generator-fn rnd size))

(defn make-size-range-seq
  [max-size]
  (cycle (range 1 max-size)))

(defn sample-seq
  ([generator] (sample-seq generator 100))
  ([generator max-size]
   (let [r (random)
         size-seq (make-size-range-seq max-size)]
     (clojure.core/map (comp rose-root (partial call-gen generator r)) size-seq))))

(defn sample
  ([generator]
   (sample generator 10))
  ([generator num-samples]
   (take num-samples (sample-seq generator))))


;; Combinators and helpers
;; ---------------------------------------------------------------------------

(defn halfs
  [n]
  (take-while (partial not= 0) (iterate #(quot % 2) n)))

(defn shrink-int
  [integer]
  (clojure.core/map (partial - integer) (halfs integer)))

(defn int-rose-tree
  [value]
  [value (clojure.core/map int-rose-tree (shrink-int value))])

(defn rand-range
  [^Random rnd lower upper]
  (let [diff (Math/abs (long (- upper lower)))]
    (if (zero? diff)
      lower
      (+ (.nextInt rnd (inc diff)) lower))))

(defn choose
  "Create a generator that returns numbers in the range
  `min-range` to `max-range`."
  [lower upper]
  (make-gen
    (fn [^Random rnd _size]
      (let [value (rand-range rnd lower upper)]
        [value (clojure.core/map int-rose-tree (shrink-int value))]))))
