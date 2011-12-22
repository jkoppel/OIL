(ns oil-programs
  (:use oil macros clojure.test))

(oil-program times [x y] []
  (oil-repeat y
     (add :output x)))

(oil-program exp [x y] [acc next]
  (setz acc)
  (oil-inc acc)
  (oil-repeat y
    (setz next)
    (oil-repeat x
      (add next acc))
    (reg= acc next))
  (reg= :output acc))

(oil-program div [x y] [count r]
  (oil-while [r (oil-gte? x y r)]
     (subtract x y)
     (oil-inc :output)))

(oil-program oil-mod [x y] [r]
  (oil-while [r (oil-gte? x y r)]
     (subtract x y))
  (reg= :output x))

(deftest multiplication
  (is (every? identity
	      (for [x (range 0 100) y (range 0 100)]
		(= (* x y) (times x y))))))


(deftest division
  (is (every? identity
	      (for [x (range 0 100) y (range 1 100)]
		(= (quot x y) (div x y))))))

(deftest modulus
  (is (every? identity
	      (for [x (range 0 100) y (range 1 100)]
		(= (mod x y) (oil-mod x y))))))


;;;(exp 2 k) does not work for k>=64 due to a bug in Clojure's overflow
;;;detection. Namely, (- 0 Long/MIN_VALUE) returns Long/MIN_VALUE, though it should
;;;overflow into BigInteger.
;;;Similar problems exist for exponentiating other powers of 2.
;;;
;;;See discussion of this issue
;;;at http://groups.google.com/group/clojure/browse_thread/thread/d48175d62f81d281
;;;
;;;Note that taking large powers of other bases works fine; they overflow into BigIntegers correctly.
(deftest exponentiation
    (is 
     (let [intpow (fn [a b] (reduce * (repeat b a)))]
       (every? identity
	       (for [x (range 0 100) y (range 0 100)
		     :when (or (= x 0)
			       (not (= 0.0 (mod (/ (Math/log x) (Math/log 2)) 1))))]
		 (= (intpow x y) (exp x y)))))))