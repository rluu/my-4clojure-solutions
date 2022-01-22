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
;;
;; Note: This solution doesn't work in clojurescript.
;; To make it work, replace 'Integer/parseInt' with 'js/parseInt'.
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
(defn power-set [input-set]
  (if (empty? input-set)
    #{#{}}
    (let [ps (power-set (rest input-set))
          first-value (first input-set)]
      (into ps (map #(conj % first-value) ps)))))

(= (power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
(= (power-set #{}) #{#{}})
(= (power-set #{1 2 3})
    #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
(= (count (power-set (into #{} (range 10)))) 1024)

;; My solution to 4Clojure problem #86.
;;
;; Note: This solution doesn't work in clojurescript.
;; To make it work, replace 'Integer/parseInt' with 'js/parseInt'.
(defn happy-number? [input-val]
  (let [digits (fn [v] (map #(Integer/parseInt %) (re-seq #"\d" (str v))))]
    (loop [v input-val
           prev-computations #{}]
      ; (println "v" v "prev-computations" prev-computations)
      (if (= v 1)
        true
        (if (contains? prev-computations v)
          false
          (as-> (digits v) x
            (map #(* % %) x)
            (reduce + x)
            (recur x (conj prev-computations v))))))))

(happy-number? 1)
(= (happy-number? 7) true)
(= (happy-number? 986543210) true)
(= (happy-number? 2) false)
(= (happy-number? 3) false)


;; My solution to 4Clojure problem #87.  (Not solved).

;; My solution to 4Clojure problem #88.
(defn my-diff [set1 set2]
  (clojure.set/union
    (clojure.set/difference set1 set2)
    (clojure.set/difference set2 set1)))

(= (my-diff #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
(= (my-diff #{:a :b :c} #{}) #{:a :b :c})
(= (my-diff #{} #{4 5 6}) #{4 5 6})
(= (my-diff #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})

;; My solution to 4Clojure problem #89.  (Not solved).

;; My solution to 4Clojure problem #90.
(defn cartesian-product [set1 set2]
  (set (apply concat
           (for [v1 set1]
             (for [v2 set2]
               [v1 v2])))))

(= (cartesian-product #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
    #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
      ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
      ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})

(= (cartesian-product #{1 2 3} #{4 5})
    #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})

(= 300 (count (cartesian-product (into #{} (range 10))
                  (into #{} (range 30)))))

;; My solution to 4Clojure problem #91.  (Not solved).
;; My solution to 4Clojure problem #92.  (Not solved).

;; My solution to 4Clojure problem #93.
(defn flatten-partially [input]
  (letfn [(is-nested? [xs] (and (sequential? xs) (sequential? (first xs))))]
    (if (is-nested? input)
      (mapcat flatten-partially input)
      (list input))))

(= (flatten-partially [["Do"] ["Nothing"]])
    [["Do"] ["Nothing"]])

(= (flatten-partially [[[[:a :b]]] [[:c :d]] [:e :f]])
    [[:a :b] [:c :d] [:e :f]])

(= (flatten-partially '((1 2)((3 4)((((5 6)))))))
    '((1 2)(3 4)(5 6)))

(if (first nil)
  true
  false)
(sequential? nil)

(sequential? [])
(sequential? '())
(true? nil)
(seq? "ryan")
(flatten [[[[:a :b]]] [[:c :d]] [:e :f]])
(def input [[[[[:a :b]] [:g :h]] [:x :y]] [[:c :d]] [:e :f]])

(first input)
(rest input)
(first (first input))
(rest (first input))

(map #(vector (* 2 %)) [1 2 3 4 5])

(seq? ["Do"])
(sequential? ["Do"])
(vector? [])
(list? '())
(coll? '())
(coll? [])
(seq? [])
(seq? '())
(rest input)

;; My solution to 4Clojure problem #94.  (Not solved).

;; My solution to 4Clojure problem #95.
(defn is-binary-tree? [lst]
  (and
    (sequential? lst)
    (= (count lst) 3)
    (some? (first lst))
    (if (nil? (second lst))
      true
      (is-binary-tree? (second lst)))
    (if (nil? (nth lst 2))
      true
      (is-binary-tree? (nth lst 2)))))

(= (is-binary-tree? '(:a (:b nil nil) nil))
    true)
(= (is-binary-tree? '(:a (:b nil nil)))
    false)
(= (is-binary-tree? [1 nil [2 [3 nil nil] [4 nil nil]]])
    true)
(= (is-binary-tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]])
    false)
(= (is-binary-tree? [1 [2 [3 [4 nil nil] nil] nil] nil])
    true)
(= (is-binary-tree? [1 [2 [3 [4 false nil] nil] nil] nil])
    false)
(= (is-binary-tree? '(:a nil ()))
    false)


;; My solution to 4Clojure problem #96.
(defn is-binary-tree-symmetric? [lst]
  (letfn [(is-binary-tree? [lst]
            (and
              (sequential? lst)
              (= (count lst) 3)
              (some? (first lst))
              (if (nil? (second lst))
                true
                (is-binary-tree? (second lst)))
              (if (nil? (nth lst 2))
                true
                (is-binary-tree? (nth lst 2)))))
          (right-leaf-first-traversal [lst]
            (if (nil? lst)
              [nil]
              [(first lst) (right-leaf-first-traversal (nth lst 2)) (right-leaf-first-traversal (second lst))]))
          (left-leaf-first-traversal [lst]
            (if (nil? lst)
              [nil]
              [(first lst) (left-leaf-first-traversal (second lst)) (left-leaf-first-traversal (nth lst 2))]))
          ]
    (if-not (is-binary-tree? lst)
      (do
        (println "Not a binary tree!")
        false)
      (let [root lst
            flattened-l (flatten (left-leaf-first-traversal (second lst)))
            flattened-r (flatten (right-leaf-first-traversal (nth lst 2)))]
        ; (println "flattened-l" flattened-l)
        ; (println "flattened-r" flattened-r)
        (= flattened-l flattened-r)))))

(= (is-binary-tree-symmetric? '(:a (:b nil nil) (:b nil nil))) true)
(= (is-binary-tree-symmetric? '(:a (:b nil nil) nil)) false)
(= (is-binary-tree-symmetric? '(:a (:b nil nil) (:c nil nil))) false)
(= (is-binary-tree-symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                                 [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true)

(= (is-binary-tree-symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                                 [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
   false)
(= (is-binary-tree-symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                                 [2 [3 nil [4 [6 nil nil] nil]] nil]])
    false)

(first [1])
(second [1])
(nth [1] 2)
(take 3 (repeatedly #(identity true)))

;; My solution to 4Clojure problem #97.
(defn pascals-triangle [level]
  (letfn [(is-last-index [coll index]
            (and (< index (count coll))
                 (= index (dec (count coll)))))
          (build-next-row [prev-row]
            (let [middle (remove nil? (for [i (range (count prev-row))]
                                        (if-not (is-last-index prev-row i)
                                          (+ (nth prev-row i) (nth prev-row (inc i))))))]
              (apply vector (concat [1] middle [1]))))
          ]
    (cond
      (< level 1) []
      (= level 1) [1]
      :else (build-next-row (pascals-triangle (dec level))))
  ))

(= (pascals-triangle 1) [1])

(= (map pascals-triangle (range 1 6))
   [     [1]
        [1 1]
       [1 2 1]
      [1 3 3 1]
     [1 4 6 4 1]])

(= (pascals-triangle 11)
   [1 10 45 120 210 252 210 120 45 10 1])

(empty? nil)

;; My solution to 4Clojure problem #98.
(defn equivalence-classes [f input-set]
  (set
    (for [[k v] (group-by f input-set)]
      (into #{} v))))

(= (equivalence-classes #(* % %) #{-2 -1 0 1 2})
    #{#{0} #{1 -1} #{2 -2}})

(= (equivalence-classes #(rem % 3) #{0 1 2 3 4 5 })
    #{#{0 3} #{1 4} #{2 5}})

(= (equivalence-classes identity #{0 1 2 3 4})
    #{#{0} #{1} #{2} #{3} #{4}})

(= (equivalence-classes (constantly true) #{0 1 2 3 4})
    #{#{0 1 2 3 4}})

;; My solution to 4Clojure problem #99.
(defn multiply-and-get-digits [x y]
  (letfn [(get-digits [v]
            (if (= v 0)
              []
              (conj (get-digits (int (/ v 10))) (mod v 10))))]
    (get-digits (* x y))
    )
  )

(= (multiply-and-get-digits 1 1) [1])
(= (multiply-and-get-digits 99 9) [8 9 1])
(= (multiply-and-get-digits 999 99) [9 8 9 0 1])


;; My solution to 4Clojure problem #100
;;
;; Note: This solution doesn't seem to work in clojurescript.
;; It seems to make the browser hang.  Probably because it is
;; evaluating the ratios/fractions as irrational numbers.
(defn least-common-multiple [& args]
  (letfn [(is-multiple [testval & args]
            (empty? (filter false? (map #(= 0 (mod testval %)) args))))]
    (loop [curr (apply min args)]
      (if (not (apply (partial is-multiple curr) args))
        (recur (+ curr (apply min args)))
        curr))))

(== (least-common-multiple 2 3) 6)
(== (least-common-multiple 5 3 7) 105)
(== (least-common-multiple 1/3 2/5) 2)
(== (least-common-multiple 3/4 1/6) 3/2)
(== (least-common-multiple 7 5/7 2 3/5) 210)

(* 1/3 2/5)

;; My solution to 4Clojure problem #101.  (Not solved).

;; My solution to 4Clojure problem #102.
(defn into-camelcase [input-str]
  (letfn [(convert-after-hyphen-chars-to-capital-letter [word]
            (let [word-length (count word)
                  characters (for [i (range word-length)]
                               (let [c (get word i)
                                     c-str (apply str [c])
                                     c-str-uppercase (clojure.string/upper-case c-str)
                                     c-uppercase (first c-str-uppercase)]
                                 (if (and (> i 0)
                                          (= \- (get word (dec i))))
                                   c-uppercase
                                   c)))]
              (->> characters
                 (filter some?)
                 (apply str))))]
    (->> input-str
      (convert-after-hyphen-chars-to-capital-letter)
      (filter #(not= \- %))
      (apply str))))

(defn into-camelcase [input-str]
  (let [words (clojure.string/split input-str #"-")
        head (first words)
        body (rest words)]
    (->> body
         (map #(clojure.string/capitalize %))
         (apply str head))))

(= (into-camelcase "something") "something")
(= (into-camelcase "multi-word-key") "multiWordKey")
(= (into-camelcase "leaveMeAlone") "leaveMeAlone")

;; Character byte operations work on the JVM, but not in Javascript.
(char (+ 1 (byte (get "ryan" 1))))
(byte \a) ;; 97
(byte \z) ;; 122
(byte \A) ;; 65
(byte \Z) ;; 90
(char (- (byte \a) 32)) ;; Turn \a to \A.

(let [word (apply str '(\r \y \a \n))] ;; Convert charcter sequence to str.
  (filter #(= \r %) word) ;; Filter characters in word.
)

(first "ryan") ;; Return first character of a string.

;; My solution to 4Clojure problem #103.
(defn k-combos [k input-set]
  (if (or (> k (count input-set)) (< k 1))
    #{}
    (letfn [(permutations [s]
              (lazy-seq
                (if (seq (rest s))
                  (apply concat (for [x s]
                                  (map #(cons x %) (permutations (remove #{x} s)))))
                  [s])))]

      ;;(println "permutations are: " (permutations (apply vector input-set)))

      (as-> input-set v
        ;; Turn the set into a vector.
        (apply vector v)
        ;; Get all permutations of the vector.
        (permutations v)
        ;; Take only the first k values of each of the permutations.
        (map #(set (take k %)) v)
        ;; Convert the list into a set to remove duplicates.
        (set v)))))

;; Another solution to problem #103 that utilizes my power-set implementation from problem #85.
(defn k-combos [k input-set]
  (letfn [(power-set [input-set]
            (if (empty? input-set)
              #{#{}}
              (let [ps (power-set (rest input-set))
                    first-value (first input-set)]
                (into ps (map #(conj % first-value) ps)))))]
    (->> input-set
      (power-set)
      (filter #(= k (count %)))
      (set))))

(= (k-combos 1 #{4 5 6}) #{#{4} #{5} #{6}})
(= (k-combos 10 #{4 5 6}) #{})
(= (k-combos 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})
(= (k-combos 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                     #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})
(= (k-combos 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}})
(= (k-combos 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                                  #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})

;; My solution to 4Clojure problem #104.
;;
;; The symbols V, L, D are never repeated in a Roman number.
;; Don’t Repeat More Than Three Times
;;
;; This solution passes all the test cases on the JVM, but for some
;; unknown reason, it is having errors when running on the 4Clojure website
;; using the Javascript Clojurescript interpreter.
;;
(defn write-roman-numeral
  ([input-int]
   (letfn [(write-roman-numeral-recursive [input-int original-input-int]
             (let [pre-result (cond
                                (nil? input-int) nil
                                (neg? input-int) nil
                                (>= input-int 1000) (str "M" (write-roman-numeral-recursive (- input-int 1000) original-input-int))
                                (>= input-int 500) (str "D" (write-roman-numeral-recursive (- input-int 500) original-input-int))
                                (>= input-int 100) (str "C" (write-roman-numeral-recursive (- input-int 100) original-input-int))
                                (>= input-int 50) (str "L" (write-roman-numeral-recursive (- input-int 50) original-input-int))
                                (>= input-int 10) (str "X" (write-roman-numeral-recursive (- input-int 10) original-input-int))
                                (>= input-int 5) (str "V" (write-roman-numeral-recursive (- input-int 5) original-input-int))
                                (>= input-int 1) (str "I" (write-roman-numeral-recursive (- input-int 1) original-input-int))
                                (= input-int 0) (str ""))
                   completed-recursion (= input-int original-input-int)]

               (if-not completed-recursion
                 pre-result
                 (if (nil? pre-result)
                   nil
                   (do
                     ; (println pre-result)
                     (-> pre-result
                         (clojure.string/replace #"DCCCC" "CM")
                         (clojure.string/replace #"CCCC" "CD")
                         (clojure.string/replace #"LXXXX" "XC")
                         (clojure.string/replace #"XXXX" "XL")
                         (clojure.string/replace #"VIIII" "IX")
                         (clojure.string/replace #"IIII" "IV")
                         (identity)
                         ))))))]
     (write-roman-numeral-recursive input-int input-int))))


(= "I" (write-roman-numeral 1))
(= "I" (write-roman-numeral 1))
(= "XXX" (write-roman-numeral 30))
(= "IV" (write-roman-numeral 4))
(= "CXL" (write-roman-numeral 140))
(= "DCCCXXVII" (write-roman-numeral 827))
(= "MMMCMXCIX" (write-roman-numeral 3999))
(= "XLVIII" (write-roman-numeral 48))


;; My solution to 4Clojure problem #105.
(defn identify-keys-and-values [input-vector]
  (loop [last-keyword nil
         remaining input-vector
         result {}]
    (if (empty? remaining)
      result
      (let [curr (first remaining)]
        (if (keyword? curr)
          (recur curr (rest remaining) (merge-with into result {curr []}))
          (recur last-keyword (rest remaining) (merge-with into result {last-keyword [curr]}))
          )))))

(second [])

(= {} (identify-keys-and-values []))
(= {:a [1]} (identify-keys-and-values [:a 1]))
(= {:a [1]
    :b [2]} (identify-keys-and-values [:a 1, :b 2]))
(= {:a [1 2 3]
    :b []
    :c [4]} (identify-keys-and-values [:a 1 2 3 :b :c 4]))

;; My solution to 4Clojure problem #106.  (Not solved).

;; My solution to 4Clojure problem #107.
(defn simple-clojure [n]
  (let [pow (fn [x n]
              (loop [acc 1
                     n n]
                (if (zero? n)
                  acc
                  (recur (* x acc) (dec n)))))]
    (fn [x] (pow x n))))

(defn simple-clojure [n]
  #(apply * (repeat n %)))

(= 256 ((simple-clojure 2) 16), ((simple-clojure 8) 2))
(= [1 8 27 64] (map (simple-clojure 3) [1 2 3 4]))
(= [1 2 4 8 16] (map #((simple-clojure %) 2) [0 1 2 3 4]))

;; My solution to 4Clojure problem #108.
;;
;; Note: This works fine on the JVM but causes the browser to hang
;; when running the test cases on the 4Clojure website with Clojurescript.
(defn lazy-search [& args]
  (loop [args args
         computed-min nil]
    (if (nil? computed-min)
      (recur args (apply min (map first args)))
      (if (apply = (map first args))
        (ffirst args)
        (recur (map #(if (and (seq %) (= computed-min (first %)))
                       (drop 1 %)
                       %)
                    args)
               nil)))))

(= 3 (lazy-search [3 4 5]))
(= 4 (lazy-search [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
(= 64 (lazy-search (map #(* % % %) (range))
                                  (filter #(zero? (bit-and % (dec %))) (range))
                                  (iterate inc 20)))
(= 7 (lazy-search (range) (range 0 100 7/6) [2 3 5 7 11 13]))


(if (seq nil) true false) ;; false
(if (seq '()) true false) ;; false
(if (seq []) true false) ;; false
(if (seq [1]) true false) ;; true
(seq '())  ;; => nil
(seq '(1)) ;; => (1)
(seq [])   ;; => nil
(seq [1])  ;; => (1)
(seq nil)  ;; => nil
(peek)
(get 1 [1234 54321])
(nth [] 0) ;; => IndexOutOfBoundsException
(nth '() 0) ;; => IndexOutOfBoundsException
(nth [1 2 3] 0) ;; => 1
(nth '(1 2 3) 0) ;; => 1
(= 3 nil) ;; false

;; My solution to 4Clojure problem #110.
(defn pronunciations
  ([input-vector]
   (lazy-seq 
     (loop [integers input-vector
            result-vector []]
       ; (println "--------------")
       (if (empty? integers)
         (do
           ; (println "Completed.  result-vector is: " result-vector)
           (cons result-vector (pronunciations result-vector)))
         (let [value (first integers)
               amount (count (take-while #(= value %) integers))]
           ; (println "integers" integers)
           ; (println "result-vector" result-vector)
           ; (println "value" value)
           ; (println "amount" amount)
           (recur (drop amount integers) (conj result-vector amount value))
           ))))))

;; A smaller, probably more idiomatic solution.
(defn pronounce-all 
  [coll]
  (letfn [(pronounce [coll] (mapcat (juxt count first)
                                    (partition-by identity coll)))]
    (iterate pronounce (pronounce coll))))

(= [[1 1] [2 1] [1 2 1 1]] (take 3 (pronunciations [1])))
(= [3 1 2 4] (first (pronunciations[1 1 1 4 4])))
(= [1 1 1 3 2 1 3 2 1 1] (nth (pronunciations [1]) 6))
(= 338 (count (nth (pronunciations [3 2]) 15)))


;; My solution to 4Clojure problem #111.  (Not solved).

;; My solution to 4Clojure problem #112.
(defn sequs [max-sum 
             nested-vector]
  (letfn [(compute [max-sum
                    curr-sum
                    remaining-structure
                    result]
            (if (empty? remaining-structure)
              [result false]
              (do
                (if (sequential? (first remaining-structure))
                  (let [inner-structure (first remaining-structure)
                        [intermediate-result is-done] (compute (- max-sum curr-sum) 0 inner-structure [])
                        intermediate-result-sum (reduce + (flatten intermediate-result))
                        new-curr-sum (+ curr-sum intermediate-result-sum)]
                    (if is-done
                      [(conj result intermediate-result) is-done]
                      (compute max-sum new-curr-sum (rest remaining-structure) (conj result intermediate-result))
                      )
                    )
                  (let [new-sum (+ curr-sum (first remaining-structure))]
                    (if (>= max-sum new-sum)
                      (compute max-sum new-sum (rest remaining-structure) (conj result (first remaining-structure)))
                      [result true]))))))]
    (let [[result is-done] (compute max-sum 0 nested-vector [])]
      result)))

(=  (sequs 10 [1 2 [3 [4 5] 6] 7])    '(1 2 (3 (4))))
(=  (sequs 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])    '(1 2 (3 (4 (5 (6 (7)))))))
(=  (sequs 9 (range))    '(0 1 2 3))
(=  (sequs 1 [[[[[1]]]]])    '(((((1))))))
(=  (sequs 0 [1 2 [3 [4 5] 6] 7])    '())
(=  (sequs 0 [0 0 [0 [0]]])    '(0 0 (0 (0))))
(=  (sequs 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
                        '(-10 (1 (2 3 (4)))))


;; My solution to 4Clojure problem #114.

