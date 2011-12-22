;;;;The core OIL interpreter.
;;;;
;;;; An OIl program and its state are represented by the following.
;;;; 1) A map from the labels of lines to the contents of the lines
;;;; 2) A map from all registers to their values, with inputs initialized appropriately
;;;; 3) A map from line labels to the label of the successor line.
;;;;
;;;; The oil function preproceses an oil program into this form and hands it to the oil-exec interpreter.
;;;;
;;;; oil-program provides syntactic sugar around defining functions using OIL programs

(ns oil
  (:use utils))

(declare exec-line oil-exec)
 
;;;Interprets a line of an OIL program. Returns the contents
;;;of the :output register upon termination.
(defn oil-exec [line lines regs succs]
  (loop [[label x y branch-label] line lines lines regs regs succs succs]
    (let [vx (regs x)
	  vy (regs y)
	  vx (- vx vy)
	  regs (assoc regs x vx)
	  next-label (if (and (= vx 0) branch-label)
		       branch-label
		       (succs label))]
      (if-let [next-line (lines next-label)]
	(recur next-line lines regs succs)
	(regs :output)))))

;;;Processes a list of input lines into a hash mapping labels to lines
(defn extract-lines [lines]
  (reduce (fn [hsh [l x y b]]
	    (if (= x :one)
	      (throw (Exception. "Register :one is read only"))
	      (assoc hsh l [l x y b])))
	  {}
	  lines))

;;;Adds a register to map regs if not alrady present
(defn conj-reg [regs r]
  (if (regs r)
    regs
    (assoc regs r 0)))

;;;Given the input and the lines of the OIL program,
;;;produces the map containing the initial values of all registers
;;;Special registers:
;;;z (initialized to 1)
;;;input registers (initialized in input hash)
;;;output
(defn extract-registers [input lines]
  (reduce
   (fn [regs [l x y b]]
     (conj-reg
      (conj-reg regs x)
      y))
   (assoc (assoc (polymap quote-symbol identity input) :one 1)
     :output
     0)
   lines))


;;;Syntax used to specify a primitive instruction
(defn sbz [label x y branch] [[label x y branch]])

;;;The oil function takes a hash giving initial values to the input registers
;;;and a sequence of instructions, and evaluates the program with the given input.
;;;
;;;Instructions come in the form of a sequence of primitive
;;;instructions. A primitive instruction is a 4-element vector
;;;[l x y b], where l is a label, x and y are registers, and b is either nil or a label
;;;A step consists of effectively running x-=y, and then continuing to the next line,
;;;unless x is set to 0 and b is truthy (not false or nil), in which case
;;;the program branches to the line with label b. If no such line exists,
;;;the program terminates.
;;;
;;;Various macros such as oil-program abstract away the details and allow
;;;the user to write code sequentially.
;;;
;;;Functions which expand into more sophisticated instruction sequences can be found in macros.clj
(defn oil [inp body]
  (let [lines (extract-lines body)
	regs (extract-registers inp body)
	successors (apply merge 
			  (map (fn [[a _ _ _] [b _ _ _]] {a b}) 
			       body
			       (rest body)))]
    (oil-exec (first body) lines regs successors)))

;;;Creates a function which runs an oil program on its arguments
(defmacro oil-program [name inputs vars & body]
  (let [args (map gensym inputs)]
    `(defn ~name ~(vec args)
       (with-genkeys ~(vec (concat vars inputs))
	 (oil ~(apply hash-map (interleave inputs (vec args)))
	      (vec (concat ~@body)))))))

