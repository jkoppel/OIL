(ns utils
  (:use clojure.pprint))

;;;Returns whether (seq x) may be executed.
;;;Loosely speaking, returns whether x is a complex (non-atomic) datatype
(defn seqable? [x]
  (isa? (class x) clojure.lang.Seqable))

;;;Generalized version of map that works on the main Clojure
;;;complex datatypes
(defn polymap [keyf valf xs]
  (cond
    (vector? xs)
      (vec (map valf xs))
    (sequential? xs)
      (map valf xs)
    (set? xs)
      (set (map valf (seq xs)))
    (map? xs)
      (apply merge (map (fn [ent]
			  {(keyf (key ent)) (valf (val ent))})
			(seq xs)))
    true
      xs))

;;;In an arbitrarily nested data structure, replaces
;;;all leaf elements x with (valf x), or, in the case of
;;;keys of maps, (keyf x)
(defn deep-polymap [keyf valf xs]
  (polymap keyf #(if (seqable? %)
		   (deep-polymap keyf valf %)
		   (valf %))
	   xs))

;;;Wraps any symbols with quote. This is helpful
;;;in using symbols passed to macros as symbols, rather
;;;than as variables
(defn quote-symbol [l]
  (if (and (symbol? l)
	   (not (= l 'quote)))
    `(quote ~l)
    l))

(defn quote-given-symbols [symbols l]
  (if (symbols l)
    `(quote ~l)`
    l))

(defn unquote-in-list [symbols l]
  (if (symbols l)
    `(unquote ~l)
    l))

(defn unquote-symbols [symbols body]
  (deep-polymap identity (partial unquote-in-list symbols) body))

(defmacro debug-expand [body]
  `(pprint (macroexpand '~body)))


;;;I have made up the term "genkey" as
;;;a portmanteau of "gensym" and "keyword"
;;;to mean a keyword (almost) guaranteed
;;;not to be used elsewhere
(defmacro with-genkeys [names & body]
  `(let ~(vec (interleave names (map (fn [nm] `(keyword (gensym '~nm))) names)))
     ~@body))