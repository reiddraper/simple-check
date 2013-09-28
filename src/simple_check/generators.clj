(ns simple-check.generators
  (:import java.util.Random)
  (:require [clj-tuple])
  (:refer-clojure :exclude [int vector list map keyword
                            char boolean byte bytes sequence]))

;; Generic helpers
;; ---------------------------------------------------------------------------

(defn sequence
  [bind-fun return-fun ms]
  (reduce (fn [acc elem]
            (bind-fun acc
                      (fn [xs]
                        (bind-fun elem
                                  (fn [y]
                                    (return-fun (conj xs y)))))))
          (return-fun [])
          ms))

;;(defn lift
;;  [bind-fun return-fun f & ms]
;;  (bind-fun (sequence bind-fun return-fun ms)
;;            (fn [args]
;;              (return-fun (rose-pure (apply f args))))))

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

(defn rose-filter
  "Takes a list of roses, not a rose"
  [pred [root children]]
  [root (clojure.core/map (partial rose-filter pred)
                          (clojure.core/filter (comp pred rose-root) children))])

(defn rose-permutations
  "Create a seq of vectors, where each rose in turn, has been replaced
  by its children."
  [roses]
  (apply concat
         (for [[rose index]
          (clojure.core/map clojure.core/vector roses (range))]
           (for [child (rose-children rose)] (assoc roses index child)))))

(defn zip-rose
  [f roses]
  "Combine two Rose trees with binary function f"
  [(apply f (clojure.core/map rose-root roses))
   (clojure.core/map (partial zip-rose f)
                     (rose-permutations roses))])

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

(defn sized
  [sized-gen]
  (make-gen
    (fn [rnd size]
      (let [sized-gen (sized-gen size)]
        (call-gen sized-gen rnd size)))))

(defn resize
  [n {gen :gen}]
  (make-gen
    (fn [rnd _size]
      (gen rnd n))))

(defn choose
  "Create a generator that returns numbers in the range
  `min-range` to `max-range`."
  [lower upper]
  (make-gen
    (fn [^Random rnd _size]
      (let [value (rand-range rnd lower upper)]
        [value (clojure.core/map int-rose-tree (shrink-int value))]))))

(defn one-of
  [generators]
  (gen-bind (choose 0 (dec (count generators)))
            #(nth generators (rose-root %))))

(defn- pick
  [[h & tail] n]
  (let [[chance gen] h]
    (if (<= n chance)
      gen
      (recur tail (- n chance)))))

(defn frequency
  [pairs]
  (let [total (apply + (clojure.core/map first pairs))]
    (gen-bind (choose 1 total)
              #(pick pairs (rose-root %)))))

(defn elements
  [coll]
  (gen-bind (choose 0 (dec (count coll)))
            #(gen-pure (rose-fmap (partial nth coll) %))))

(defn such-that
  [pred gen]
  (make-gen
    (fn [rand-seed size]
      (let [value (call-gen gen rand-seed size)]
        (if (pred (rose-root value))
          (rose-filter pred value)
          (recur rand-seed (+ 1 size)))))))

(defn tuple
  [& generators]
  (gen-bind (sequence gen-bind gen-pure generators)
            (fn [roses]
              (gen-pure (zip-rose clojure.core/vector roses)))))

(def int
  "Really returns a long"
  (sized (fn [size] (choose (- size) size))))

(def nat
  (fmap #(Math/abs (long %)) int))

(def pos-int nat)

(def neg-int (fmap (partial * -1) nat))

(def s-pos-int
  (fmap inc nat))

(def s-neg-int
  (fmap dec neg-int))
