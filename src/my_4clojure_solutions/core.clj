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
(defn totient [n]
  (let [get-greatest-common-divisor (fn [a b] 
                                      (->> (max a b)
                                           (range 1)
                                           (reverse)
                                           (drop-while #(not (= 0 (mod a %) (mod b %))))
                                           (first)))
        is-coprime? (fn [a b] (= 1 (get-greatest-common-divisor a b)))]
      (cond 
        (< n 1) 0
        (= n 1) 1
        (> n 1) (->> (range 1 n)
                     (map #(is-coprime? % n))
                     (filter true?)
                     (count)))))

(= (totient 1) 1)
(= (totient 10) (count '(1 3 7 9)) 4)
(= (totient 40) 16)
(= (totient 99) 60)

;; My solution to 4Clojure problem #77.
(defn anagrams [words]
  (loop [m {}
         remaining-words words]
    (if (not (empty? remaining-words))
      (let [str-to-char-set #(into #{} %)
            word (first remaining-words)
            k (str-to-char-set word)
            anagram-word-set (if (nil? (m k)) #{} (m k))]
        (recur (assoc m k (conj anagram-word-set word)) (rest remaining-words)))
      ; Return results.
      (->> m
        (map (fn [[k v]] v))
        (filter #(> (count %) 1))
        (set)))))

(= (anagrams ["meat" "mat" "team" "mate" "eat"])
    #{#{"meat" "team" "mate"}})

(= (anagrams ["veer" "lake" "item" "kale" "mite" "ever"])
    #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})

(defn better-anagrams [words]
  (->>
    (group-by frequencies words)
    (map (fn [[k v]] (set v)))
    (filter #(> (count %) 1))
    (set)))

(doc frequencies)
(group-by frequencies ["meat" "mat" "team" "mate" "eat"])

(= (better-anagrams ["meat" "mat" "team" "mate" "eat"])
    #{#{"meat" "team" "mate"}})

(= (better-anagrams ["veer" "lake" "item" "kale" "mite" "ever"])
    #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})

;; My solution to 4Clojure problem #78.
(doc trampoline)
(fn?)

(defn my-trampoline
  ([f] (if (fn? f) (recur (f)) f))
  ([f & args] (loop [rv (if (fn? f) (apply f args) f)]
                (if (fn? rv) (recur (rv)) rv))))


(= (letfn [(triple [x] #(sub-two (* 3 x)))
           (sub-two [x] #(stop?(- x 2)))
           (stop? [x] (if (> x 50) x #(triple x)))]
     (my-trampoline triple 2))
   82)

(= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
           (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
     (map (partial my-trampoline my-even?) (range 6)))
   [true false true false true false])

;; My solution to 4Clojure problem #79.

; Steps
; - Method to build paths that include the next level of the tree.
; - Method to build all paths of a tree for height h.
; - Method to fetch all values along a path on a tree.
; - Get all paths.
; - Get the value combination for each path.
; - Get the sums of each value combination.
; - Get minimum of these sums.
;
(defn process-tree [tree]
  (letfn [(next-level-paths [paths]
            (let [updated-paths (for [path paths]
                                  (let [last-path (last path)]
                                    ;(println "path: " path)
                                    ;(println "last-path: " last-path)
                                    ;(println "inc last-path: " (inc last-path))
                                    [(conj path last-path) (conj path (inc last-path))]))]
              ;(println "updated-paths: " updated-paths)
              ;(println "apply concat updated-paths: " (apply concat updated-paths))
              (apply concat updated-paths)
              ))
          (all-paths [tree-height]
            (nth (iterate next-level-paths [[0]]) (dec tree-height)))
          (values-along-path [tree path]
            (map nth tree path)
            )
          ]
    ;(println "all-paths[1]: " (all-paths 1))
    ;(println "all-paths[2]: " (all-paths 2))
    ;(nth (iterate next-level-paths [[0]]) 0)
    ;(nth (iterate next-level-paths [[0]]) 1)
    ;(nth (iterate next-level-paths [[0]]) 2)
    ;(nth (iterate next-level-paths [[0]]) 3)

    (->> (count tree)
      ; Get all paths for a tree height.
      (all-paths)
      ; Get all potential value sequences for all paths.
      (map (partial values-along-path tree))
      ; Compute the sum of each of those value sequences.
      (map #(apply + %))
      ; Get the minmium sum.
      (apply min)
      )))

(let [tree [[1]
           [9 4]]
      ]
  (process-tree tree))

(= (process-tree [   [1]
                    [2 4]
                   [5 1 4]
                  [2 3 4 5]])
   (+ 1 2 1 3)
   7)

(= (process-tree [     [3]
                      [2 4]
                     [1 9 3]
                    [9 9 2 4]
                   [4 6 6 7 8]
                  [5 7 3 5 1 4]])
    (+ 3 4 3 2 7 1)
    20)

;; Solution that I found the easiest to understand. 
(def process-triangle
  (letfn [(next-steps [paths]
            (apply concat
                   (for [path paths, :let [last-step (last path)]]
                     [(conj path last-step) 
                      (conj path (inc last-step))]
                     )))
          (all-paths [length]
            (nth (iterate next-steps [[0]]) (dec length)))
          (values-along-path [triangle path]
            (map nth triangle path))]
    (fn [triangle]
      (let [paths         (all-paths (count triangle))
            values        (map (partial values-along-path triangle) paths)
            summed-values (map (partial apply +) values)]
        ;(println "all-paths: " paths)
        ;(println "values: " values)
        ;(println "summed-values: " summed-values)
        (apply min summed-values)))))


(def empty-seq '())
(def nil-seq nil)
(def empty-vector [])
(def nil-vector nil)

(if empty-seq true false)
(if empty-vector true false)
(if nil-seq true false)
(if nil-vector true false)
(if-not (empty? empty-seq) true false)
(if-not (empty? empty-vector) true false)
(if-not nil-seq true false)
(if-not nil-vector true false)
(empty? nil-seq)
(empty? nil-vector)
(empty? nil)

(keyword "3")

(for [n (range (count tree))]
  [x y])


;; My solution to 4Clojure problem #80
(defn perfect-number? [n]
  (let [is-divisor (fn [n t] (= 0 (mod n t)))]
    (->> (range 1 n)
         (filter #(is-divisor n %))
         (apply +)
         (= n))))

(= (perfect-number? 6) true)
(= (perfect-number? 7) false)
(= (perfect-number? 496) true)
(= (perfect-number? 500) false)
(= (perfect-number? 8128) true)

;; My solution to 4Clojure problem #81
(defn my-intersection [set1 set2]
  (set (filter #(contains? set2 %) set1)))

(= (my-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3})
(= (my-intersection #{0 1 2} #{3 4 5}) #{})
(= (my-intersection #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})

;; My solution to 4Clojure problem #82.  (Effort made, but not solved).
;; I realized after writing this below that reordering of characters is not allowed between words.  So the solution fails test case 3.

(defn contains-word-chain? [word-set]
  (letfn [(word-chain? [word1 word2]
            (loop [word1freq (frequencies word1)
                   word2freq (frequencies word2)
                   letter-count-in-word1-not-in-word2 0
                   letter-count-in-word2-not-in-word1 0]
              ; (println "TOP of count-diff loop."
              ;          "word1freq:" word1freq
              ;          "word2freq:" word2freq
              ;          "letter-count-in-word1-not-in-word2:" letter-count-in-word1-not-in-word2
              ;          "letter-count-in-word2-not-in-word1:" letter-count-in-word2-not-in-word1)
              (let [is-word-chain-so-far 
                    (if (or (= 1 letter-count-in-word1-not-in-word2 letter-count-in-word2-not-in-word1)
                            (and (= 1 letter-count-in-word1-not-in-word2) (= 0 letter-count-in-word2-not-in-word1))
                            (and (= 0 letter-count-in-word1-not-in-word2) (= 1 letter-count-in-word2-not-in-word1)))
                      true
                      false)]
                (cond 
                  (and (empty? word1freq) (empty? word2freq)) is-word-chain-so-far
                  (and (empty? word1freq) (not (empty? word2freq))) (recur word2freq word1freq letter-count-in-word2-not-in-word1 letter-count-in-word1-not-in-word2)
                  :else (let [[k v1] (first word1freq)
                              v2 (if (contains? word2freq k) (word2freq k) 0)]
                          ; (println "k:" k "v1:" v1 "v2:" v2)
                          (cond 
                            (= v1 v2) (recur (dissoc word1freq k) (dissoc word2freq k) letter-count-in-word1-not-in-word2 letter-count-in-word2-not-in-word1)
                            (< v1 v2) (recur (dissoc word1freq k) (dissoc word2freq k) letter-count-in-word1-not-in-word2 (+ (- v2 v1) letter-count-in-word2-not-in-word1))
                            (> v1 v2) (recur (dissoc word1freq k) (dissoc word2freq k) (+ (- v1 v2) letter-count-in-word1-not-in-word2) letter-count-in-word2-not-in-word1)
                            ))))))
          (is-word-chain [word-seq]
            ; (println "Examining for a word chain: " word-seq)
            (loop [words word-seq]
              (if (<= (count words) 1)
                (do 
                  ; (println "Result: true")
                  true)
                (let [test-next-two-words (word-chain? (first words) (second words))]
                  ; (println "test-next-two-words:" test-next-two-words)
                  (if test-next-two-words
                    (recur (rest words))
                    (do 
                      ; (println "Result: false")
                      false))
                  )
                )))
          (get-permutations [s]
            (lazy-seq
              (if (seq (rest s))
                (apply concat (for [x s]
                                (map #(cons x %) (get-permutations (remove #{x} s)))))
                [s])))
          ]
    ; (println "word1:" word1 "word2:" word2 "diff-count:" (count-diff word1 word2))
    (let [permutations (get-permutations word-set)
          word-chains (filter is-word-chain permutations)
          final-result (not (empty? word-chains))]
      ; (println "permutations" permutations)
      ; (println "word chains: " word-chains)
      ; (println "final result: " final-result)
      final-result)))

(contains-word-chain? #{"dot" "pot"})

(contains-word-chain? #{"do"})
(contains-word-chain? #{"do" "dot"})
(contains-word-chain? #{"do" "dot" "pot"})
(contains-word-chain? #{"do" "dot" "pot" "spot"})
(contains-word-chain? #{"do" "dot" "pot" "spot" "pout"})
(contains-word-chain? #{"do" "dot" "pot" "spot" "pout" "spout"})

(= true (contains-word-chain? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
(= false (contains-word-chain? #{"cot" "hot" "bat" "fat"}))
(= false (contains-word-chain? #{"to" "top" "stop" "tops" "toss"}))
(= true (contains-word-chain? #{"spout" "do" "pot" "pout" "spot" "dot"}))
(= true (contains-word-chain? #{"share" "hares" "shares" "hare" "are"}))
(= false (contains-word-chain? #{"share" "hares" "hare" "are"}))


(let [[k v1] (first (frequencies "ryan"))]
  (println "k:" k ", v1:" v1)
  ((frequencies "ryanr") k)
  )
(first (frequencies "ryan"))
(frequencies "ryana")

;; My solution to 4Clojure problem #83.
(defn problem-83 [& bools]
  (if (and (contains? (set bools) true) (not (= #{true} (set bools)))) true false))

(= false (problem-83 false false))
(= true (problem-83 true false))
(= false (problem-83 true))
(= true (problem-83 false true false))
(= false (problem-83 true true true))
(= true (problem-83 true true true false))

;; My solution to 4Clojure problem #84.  (Not solved).

;; My solution to 4Clojure problem #85.


