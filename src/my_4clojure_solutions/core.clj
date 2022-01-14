(ns my-4clojure-solutions.core)


(println "this works")
(+ 3 4 5)
(map inc '(1 2 3 4))


;; My solution to 4Clojure problem #53.
(defn longest-consecutive-increasing-sub-seq [coll]
  (loop [longest []
         curr []
         last-v nil
         remaining coll]
    (let [v (first remaining)]
      (if (nil? v)
        longest
        (if (nil? last-v)
            (recur longest
                   (conj curr v)
                   v
                   (rest remaining))
            (if (> v last-v)
              (if (< (count longest) (inc (count curr)))
                (recur (conj curr v) 
                       (conj curr v)
                       v
                       (rest remaining))
                (recur longest
                       (conj curr v)
                       v
                       (rest remaining)))
              (recur longest
                     [v]
                     v
                     (rest remaining))))))))

(= (longest-consecutive-increasing-sub-seq [1 0 1 2 3 0 4 5]) [0 1 2 3])
(= (longest-consecutive-increasing-sub-seq [5 6 1 3 2 7]) [5 6])
(= (longest-consecutive-increasing-sub-seq [2 3 3 4 5]) [3 4 5])
(= (longest-consecutive-increasing-sub-seq [7 6 5 4]) [])

(defn longest-consecutive-increasing-sub-seq [coll]
  (let [is-increasing (fn [s] (> (second s) (first s)))
        is-not-increasing (complement is-increasing)]
    (as->
      coll v
      (partition 2 1 v)
      (partition-by is-not-increasing v)
      (if (is-increasing (first (first v))) (take-nth 2 v) (take-nth 2 (rest v)))
      (sort-by count v)
      (let [max-count (-> v (last) (count))]
        (filter #(= (count %) max-count) v))
      (first v)
      (flatten v)
      (distinct v)
    )))

(= (longest-consecutive-increasing-sub-seq [1 0 1 2 3 0 4 5]) [0 1 2 3])
(= (longest-consecutive-increasing-sub-seq [5 6 1 3 2 7]) [5 6])
(= (longest-consecutive-increasing-sub-seq [2 3 3 4 5]) [3 4 5])
(= (longest-consecutive-increasing-sub-seq [7 6 5 4]) [])


;; My solution to 4Clojure problem #58.
(defn compose [& functions]
  (let [ordered-functions (reverse functions)]
    (fn f [& args]
      (loop [curr-function (first ordered-functions)
             remaining-functions (rest ordered-functions)
             loop-args args]
        (if (empty? remaining-functions)
          (do
            (apply curr-function loop-args))
          (do
            (recur
             (first remaining-functions)
             (rest remaining-functions)
             (list (apply curr-function loop-args)))))))))

((compose rest reverse) [1 2 3 4])
(= 5 ((compose (partial + 3) second) [1 2 3 4]))
(= true ((compose zero? #(mod % 8) +) 3 5 7 9))

((compose #(mod % 8) +) 3 5 7 9)


;; My solution to 4Clojure problem #59.
(defn my-juxt [& functions]
  (fn [& args]
    (map #(apply % args) functions)))

(= [21 6 1] ((my-juxt + max min) 2 3 5 1 6 4))
(= ["HELLO" 5] ((my-juxt #(.toUpperCase %) count) "hello"))
(= [2 6 4] ((my-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))

;; My solution to 4Clojure problem #60.
(defn my-reductions
  ([f coll] (my-reductions f (first coll) (rest coll)))
  ([f initial-input coll] (lazy-seq
                           (cons
                            initial-input
                            (if (not (empty? coll))
                              (my-reductions f (f initial-input (first coll)) (rest coll)))))))


(take 5 (my-reductions + (range)))


;; My solution to 4Clojure problem #61.
(defn my-zipmap [keys values]
  (let [size (min (count keys) (count values))]
    (loop [i 0
           result {}]
      (if (< i size)
        (recur (inc i) (assoc result (nth keys i) (nth values i)))
        result))))
;; A better solution to problem #61.
(defn better-zipmap [keys values]
  (apply hash-map (interleave keys values)))

(my-zipmap [:a :b :c] [1 2 3])
(my-zipmap [1 2 3 4] ["one" "two" "three"])
(my-zipmap [:foo :bar] ["foo" "bar" "baz"])

(defn my-test [keys values]
  (map vector keys values))

(my-test [:a :b :c] [1 2 3])
(my-test [1 2 3 4] ["one" "two" "three"])
(my-test [:foo :bar] ["foo" "bar" "baz"])

(defn another-zipmap [keys values]
  (into {} (map vector keys values)))

(another-zipmap [:a :b :c] [1 2 3])
(another-zipmap [1 2 3 4] ["one" "two" "three"])
(another-zipmap [:foo :bar] ["foo" "bar" "baz"])



;; My solution to 4Clojure problem #62.
(defn my-iterate [f x]
  (lazy-seq (cons x (my-iterate f (f x)))))

(take 5 (my-iterate #(* 2 %) 1))
(= (take 5 (my-iterate #(* 2 %) 1))
   [1 2 4 8 16])

(take 100 (my-iterate inc 0))
(= (take 100 (my-iterate inc 0))
   (take 100 (range)))

(take 9 (my-iterate #(inc (mod % 3)) 1))
(= (take 9 (my-iterate #(inc (mod % 3)) 1))
   (take 9 (cycle [1 2 3])))

;; My solution to 4Clojure problem #63.
(defn my-groupby [f s]
  (loop [s s
         result {}]
    (if (empty? s)
      result
      (let [key (f (first s))
            value (first s)]
        (if (nil? (result key))
          (recur (rest s) (assoc result key [value]))
          (recur (rest s) (assoc result key (conj (result key) value)))
          )))))



(my-groupby #(> % 5) #{1 3 6 8})
(= (my-groupby #(> % 5) #{1 3 6 8}) {false [1 3], true [6 8]})

(= (my-groupby #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})

(= (my-groupby count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})

;; My solution to 4Clojure problem #65.
(defn my-get-coll-type [coll]
  (cond
    (and (associative? coll) (not (reversible? coll)) (ifn? coll)) :map
    (and (associative? coll) (reversible? coll) (ifn? coll)) :vector
    (and (not (associative? coll)) (not (reversible? coll)) (not (ifn? coll))) :list
    (and (not (associative? coll)) (not (reversible? coll)) (ifn? coll)) :set
    :else :unknown))

(associative? {}) ;; => true
(associative? []) ;; => true
(associative? '()) ;; => false
(associative? #{}) ;; => false

(reversible? {}) ;; => false
(reversible? []) ;; => true
(reversible? '()) ;; => false
(reversible? #{}) ;; => false

(ifn? {}) ;; => true
(ifn? []) ;; => true
(ifn? '()) ;; => false
(ifn? #{}) ;; => true

(counted? {}) ;; true
(counted? []) ;; true
(counted? '()) ;; true
(counted? #{}) ;; true

(sequential? {}) ;; false
(sequential? []) ;; true
(sequential? '()) ;; true
(sequential? #{}) ;; false

;; (let [random-value 3
;; collsize (count coll)]
;; (= (+ collsize 2) (count (into (into coll [random-value]) [random-value]))) :list


;; My solution to 4Clojure problem #66.
(defn my-greatest-common-divisor [x y]
  (let [is-common-divisor? (fn [x y d]
                             (and (= 0 (mod x d))
                                  (= 0 (mod y d))))]
    (apply max (map
                #(if (is-common-divisor? x y %) % 1)
                (range (min x y) 0 -1)))))

(= (my-greatest-common-divisor 2 4) 2)
(= (my-greatest-common-divisor 10 5) 5)
(= (my-greatest-common-divisor 5 7) 1)
(= (my-greatest-common-divisor 1023 858) 33)

(defn another-greatest-common-divisor [x y]
  (let [is-common-divisor? (fn [x y d]
                             (and (= 0 (mod x d))
                                  (= 0 (mod y d))))]
    (apply max (filter #(is-common-divisor? x y %) (range (min x y) 0 -1)))))

(= (another-greatest-common-divisor 2 4) 2)
(= (another-greatest-common-divisor 10 5) 5)
(= (another-greatest-common-divisor 5 7) 1)
(= (another-greatest-common-divisor 1023 858) 33)

;; My solution to 4Clojure problem #67.
(defn primes [n]
  (let [isprime? (fn [x]
                   (if (< 2) false)
                   (loop [i 2]
                     (if (>= i x)
                       true
                       (if (zero? (mod x i))
                         false
                         (recur (inc i))))))
        get-next-prime (fn [prev-prime]
                         (loop [x (inc prev-prime)]
                           (if (isprime? x) x (recur (inc x)))))
        prime-lazy-seq (fn prime-lazy-seq
                         ([] (prime-lazy-seq 2))
                         ([prev-prime]
                          (lazy-seq
                           (cons
                            prev-prime
                            (prime-lazy-seq (get-next-prime prev-prime))))))]
    (take n (prime-lazy-seq))))


(= (primes 2) [2 3])
(= (primes 5) [2 3 5 7 11])
(= (last (primes 100)) 541)

;; My solution to 4Clojure problem #69.
(defn my-merge-with [f first-map & other-maps]
  (reduce
    (fn [acc kv-seq]
      (let [[k v] kv-seq
            acc-value (acc k)]
        (if (some? acc-value)
          (assoc acc k (f acc-value v))
          (assoc acc k v))))
    first-map
    (apply concat other-maps)))

(= (another-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})

(= (another-merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15})

(= (another-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   {:a [3 4 5], :b [6 7], :c [8 9]})


;; My solution to 4Clojure problem #70.
(defn my-sorted-split-without-punctuation [input-str]
  (require '[clojure.string])
  (let [my-comparator (fn [a b]
                        (let [a-lowercase (clojure.string/lower-case a)
                              b-lowercase (clojure.string/lower-case b)]
                          (compare a-lowercase b-lowercase)))
        remove-punctuation #(clojure.string/replace % #"[^a-zA-Z]" "")]
    (sort my-comparator
          (map remove-punctuation (clojure.string/split input-str #" ")))))

(doc sort)
(doc compare)
(doc clojure.string/split)
(doc clojure.string/replace)
(doc clojure.string/lower-case)

(my-sorted-split-without-punctuation "Ryan went to work today, like a dummy!")

(sort '(5 3 1))
(sort '("c" "b" "a"))
(sort ["c" "b" "a"])

(= (my-sorted-split-without-punctuation  "Have a nice day.")
    ["a" "day" "Have" "nice"])

(= (my-sorted-split-without-punctuation  "Clojure is a fun language!")
    ["a" "Clojure" "fun" "is" "language"])

(= (my-sorted-split-without-punctuation  "Fools fall for foolish follies.")
    ["fall" "follies" "foolish" "Fools" "for"])

(defn another-sorted-split-without-punctuation [input-str]
  (require '[clojure.string])
  (let [remove-punctuation #(clojure.string/replace % #"[^a-zA-Z]" "")]
    (sort-by #(.toLowerCase %) (map remove-punctuation (re-seq #"\w+" input-str)))))

(defn another-sorted-split-without-punctuation [input-str]
  (require '[clojure.string])
  (let [remove-punctuation #(clojure.string/replace % #"[^a-zA-Z]" "")]
    (sort-by clojure.string/lower-case (map remove-punctuation (re-seq #"\w+" input-str)))))

(doc re-seq)
(doc sort-by)

(= (another-sorted-split-without-punctuation  "Have a nice day.")
    ["a" "day" "Have" "nice"])

(= (another-sorted-split-without-punctuation  "Clojure is a fun language!")
    ["a" "Clojure" "fun" "is" "language"])

(= (another-sorted-split-without-punctuation  "Fools fall for foolish follies.")
    ["fall" "follies" "foolish" "Fools" "for"])


;; My solution to 4Clojure problem #73.
(defn get-tic-tac-toe-winner [board]
  (let [get-winner (fn [row]
                     (if (and (not= :e (first row)) 
                              (= (first row) (second row) (last row)))
                       (first row)
                       nil))
        combos (list (first board)
                     (second board)
                     (last board)
                     (map first board)
                     (map second board)
                     (map last board)
                     (vector (first (first board)) (second (second board)) (last (last board)))
                     (vector (first (last board)) (second (second board)) (last (first board))))]
    (->> combos
         (map get-winner)
         (drop-while nil?)
         (first))))

(= nil (get-tic-tac-toe-winner [[:e :e :e]
                                [:e :e :e]
                                [:e :e :e]]))

(= :x (get-tic-tac-toe-winner [[:x :e :o]
                               [:x :e :e]
                               [:x :e :o]]))

(= :o (get-tic-tac-toe-winner [[:e :x :e]
                               [:o :o :o]
                               [:x :e :x]]))

(= nil (get-tic-tac-toe-winner [[:x :e :o]
                                [:x :x :e]
                                [:o :x :o]]))

(= :x (get-tic-tac-toe-winner [[:x :e :e]
                               [:o :x :e]
                               [:o :e :x]]))

(= :o (get-tic-tac-toe-winner [[:x :e :o]
                               [:x :o :e]
                               [:o :e :x]]))

(= nil (get-tic-tac-toe-winner [[:x :o :x]
                                [:x :o :x]
                                [:o :x :o]]))


;; My solution to 4Clojure problem #74.
(defn string-squares [input-str]
  (as-> input-str v
    (clojure.string/split v #",")
    (map #(Integer/parseInt %) v)
    (filter #(= (* (int (Math/sqrt %)) (int (Math/sqrt %))) %) v)
    (map str v)
    (clojure.string/join "," v)))

(= (string-squares "4,5,6,7,8,9") "4,9")
(= (string-squares "15,16,25,36,37") "16,25,36")


(re-seq #"\d" "3")
(Integer. "3")         ;; Equivalent to Java: new Integer("3")
(Integer/parseInt "3") ;; Equivalent to Java: Integer.parseInt("3")
(Integer/valueOf "3")  ;; Equivalent to Java: Integer.valueOf("3")


;; My solution to 4Clojure problem #75.


