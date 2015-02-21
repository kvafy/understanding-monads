;; Excercise: Multivalued functions

; Suppose we want to compute with complex numbers. Now, nth-root of a complex
; number isn't actually just one number but, in fact, set of n numers. Rings
; a bell?


; First let's define complex number as a datatype and define the nth-root function.

(defrecord Complex [mag phi])

(defn nth-root [n x]
  (let [two-pi (* 2 Math/PI)
        mag-new (Math/pow (:mag x) (/ 1 n))
        phis-new (map #(+ (:phi x) (/ (* % two-pi) n)) (range n))]
    (map #(Complex. %1 %2) (repeat mag-new) phis-new)))


; We usually need square and cubic roots the most often, so why not to define
; them?

; sqrt,cbrt :: Complex -> [Complex]

(def sqrt
  (partial nth-root 2))

(def cbrt
  (partial nth-root 3))


; Now, what about sixth root? We know we can mathematically compute it as
; (n ^ (1/3)) ^ (1/2). We could use composition of sqrt and cbrt. See the
; problem? The functions take a single value but produce whole list of values.
; We need special function that will enable us to chain the functions.
; It will be "bind":

; bind :: Complex -> [Complex]

(defn bind [fx g]
  (mapcat g fx))

(let [x (Complex. 64 0)]
  (bind (sqrt x) cbrt))
; => [(Complex. 2 0) ... ]


; For the sake of excercise, let's also implement identity/unit function,
; aka. return. This function takes a single number and makes a monadic
; value out of it.

(defn unit [x]
  [x])

(unit (Complex. 1 0))
; => [(Complex. 1 0)]


; Now, we also want to normalize our complex numbers.

; abs :: Complex -> Complex
(defn abs [x]
    (Complex. (:mag x) 0))

; But normalization doesn't produce a list of values... There is a missmatch
; between signature of the nth-root function, which returns collection of
; numbers, and abs, which returns just a single value. How do we plug-in the
; abs function into our chaining/combining using bind?
; Yes, we will create "lift" function, which will make the abs function
; return list of values (of a single value actually).

(defn lift [f]
  #(vector (f %)))



(let [lifted-abs (lift abs)
      x (Complex. 25 0)]
  (bind (sqrt x) lifted-abs))
; => [(Complex. 5 0) (Complex. 5 0)]


; Notice we didn't touch implementation of "abs" and still we were able to
; plug abs into the chaining/combination using "bind".


; To summarize... Monad is the list, the wrapping of multiple complex numbers
; into a single values. So if x is a number, then [x] is a list monad
; encapsulating complex numbers. The bind operator lets us chain functions
; that take a simple input and produce a monad. We implemented bind, so that
; it unwraps each element of the list monad, applies the transformation
; function onto the value producing a whole list of values (a monad)
; and finally combining results of all the invocations into a single list.