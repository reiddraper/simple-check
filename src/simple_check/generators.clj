(ns simple-check.generators
  (:import java.util.Random)
  (:require [clj-tuple])
  (:refer-clojure :exclude [int vector list hash-map map keyword
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
  [(apply f (clojure.core/map rose-root roses))
   (clojure.core/map (partial zip-rose f)
                     (rose-permutations roses))])

(defn remove-roses
  [roses]
  (concat
    [(rest roses)]
    [(drop-last roses)]
    (for [child (rose-children (first roses))]
      (cons child (rest roses)))))

(defn shrink-rose
  [f roses]
  (if (seq roses)
    [(apply f (clojure.core/map rose-root roses))
     (clojure.core/map (partial shrink-rose f) (remove-roses roses))]
    [(f) []]))

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

(defn return
  [value]
  (gen-pure (rose-pure value)))

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

(def boolean
  (elements [false true]))

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

(defn vector
  ([generator]
  (gen-bind
    (sized #(choose 0 %))
    (fn [num-elements-rose]
      (gen-bind (sequence gen-bind gen-pure
                          (repeat (rose-root num-elements-rose)
                                  generator))
                (fn [roses]
                  (gen-pure (shrink-rose clojure.core/vector
                                         roses)))))))
  ([generator num-elements]
   (apply tuple (repeat num-elements generator)))
  ([generator min-elements max-elements]
   (gen-bind
     (choose min-elements max-elements)
     (fn [num-elements-rose]
       (gen-bind (sequence gen-bind gen-pure
                           (repeat (rose-root num-elements-rose)
                                   generator))
                 (fn [roses]
                   (gen-bind
                     (gen-pure (shrink-rose clojure.core/vector
                                            roses))
                     (fn [rose]
                       (gen-pure (rose-filter
                                   (fn [v] (and (>= (count v) min-elements)
                                                (<= (count v) max-elements))) rose))))))))))

(defn list
  [generator]
  (gen-bind (sized #(choose 0 %))
            (fn [num-elements-rose]
              (gen-bind (sequence gen-bind gen-pure
                                  (repeat (rose-root num-elements-rose)
                                          generator))
                        (fn [roses]
                          (gen-pure (shrink-rose clojure.core/list
                                                 roses)))))))

(def byte (fmap clojure.core/byte (choose 0 127)))

(def bytes (fmap clojure.core/byte-array (vector byte)))

(defn map
  [key-gen val-gen]
  (let [input (vector (tuple key-gen val-gen))]
    (fmap (partial into {}) input)))

(def hash-map map)

(def char
  "Generates character from 0-255."
  (fmap clojure.core/char (choose 0 255)))

(def char-ascii
  "Generate only ascii character."
  (fmap clojure.core/char (choose 32 126)))

(def char-alpha-numeric
  "Generate alpha-numeric characters."
  (fmap clojure.core/char
        (one-of [(choose 48 57)
                 (choose 65 90)
                 (choose 97 122)])))

(def string
  (fmap clojure.string/join (vector char)))

(def string-ascii
  (fmap clojure.string/join (vector char-ascii)))

(def string-alpha-numeric
  (fmap clojure.string/join (vector char-alpha-numeric)))

(def keyword
  "Generate keywords."
  (->> string-alpha-numeric
    (such-that #(not= "" %))
    (fmap clojure.core/keyword)))

(def ratio
  (->> (tuple int (such-that (complement zero?) int))
    (fmap (fn [[a b]] (/ a b)))))
