;;;;This file used to contain Clojure macros which implement OIL macros.
;;;;Now it contains Clojure functions which implement OIL macros.
;;;;
;;;;Many functions take as an argument a register to return a value in


(ns macros
  (:use oil utils))

(defmacro def-oil-syntax [name args local-vars & body]
  `(defn ~name ~args
     (with-genkeys ~local-vars
       (vec
	(concat ~@body)))))

(def-oil-syntax setz [x] [l1]
  (sbz l1 x x nil))


(def-oil-syntax reg= [x y] [l1 l2 r1]
  (setz r1)
  (setz x)
  (sbz l1 r1 y nil)
  (sbz l2 x r1 nil))

(def-oil-syntax oil-zero? [x b] [l r]
  (sbz l x r b))

;;;;Next three instructions are equivalent to the instructions of the register machine
;;;;presented in class

(def-oil-syntax oil-inc [x] [l1 l2 r]
  (setz r)
  (sbz l1 r :one nil)
  (sbz l2 x r nil))

(def-oil-syntax oil-dec [x b] [l r1]
  (oil-zero? x b)
  (sbz l x :one nil))

(def-oil-syntax halt [] [l1 r1 l2]
  (sbz l1 r1 r1 l2))

(def-oil-syntax add [x y] [l1 l2 r1]
  (setz r1)
  (sbz l1 r1 y nil)
  (sbz l2 x r1 nil))

(def-oil-syntax subtract [x y] [l]
  (sbz l x y nil))

(def-oil-syntax goto [b] [l r]
  (sbz l r r b))

;;;Creates a dummy line for goto purposes
(def-oil-syntax label [l] [r]
  (sbz l r r nil))

;;;Repeats body X times
(def-oil-syntax oil-repeat [X & body] [init break i]
  (reg= i X)
  (label init)
  (oil-dec i break)
  (apply concat body)
  (goto init)
  (label break))
   
(def-oil-syntax oil-nonneg? [x ret] [g l r yes no start end]
  (oil-zero? x yes)

  (reg= g x)
  (reg= l x)

  (label start)
  (oil-dec l nil)
  (oil-inc g)

  (oil-zero? l yes)
  (oil-zero? g no)
  (goto start)

  (label no)
  (setz ret)
  (goto end)

  (label yes)
  (setz ret)
  (oil-inc ret)
  (goto end)
  (label end))

(def-oil-syntax oil-gte? [x y ret] [r]
  (reg= r x)
  (subtract r y)
  (oil-nonneg? r ret))


(def-oil-syntax oil-while [[r test] & body] [start end]
  (label start)
  test
  (oil-zero? r end)
  (apply concat body)
  (goto start)
  (label end))