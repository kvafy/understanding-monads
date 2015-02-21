;; Excercise: Debuggable functions as a monad

; Consider we have some functions f and g and we want them to produce some kind
; of debug messages as they get invoked.

; f,g :: Number -> Number

(defn f [x]
  (* 2 x))

(defn g [x]
  (inc x))


; To stay in the domain of pure functions without side effects, as all decent
; people should, need to make the functions produce tuples [Number, String]
; rather than simply numbers. This can be done as follows:

; f',g' :: Number -> (Number, String)

(defn f' [x]
  [(* 2 x) "times two was called"])

(defn g' [x]
  [(inc x) "plus one was called"])


; But what if we want to compose f' and g'? Ie. to compute (g' (f' <something>)).
; We cannot, because g' expects just a number as input, whereas f' produces a 2-tuple
; [Number, String]. Before feeding result of f' to g', we need to destructure the output
; of f' into [fx fs]. Then we feed fx to g' producing [gx gs] and overall return
; [gx (string/join fs gs)] as the result of the composition. We will use "bind" function
; to achieve just that (bind defines the plumbing which allows us to chain/combine
; the debuggable functions):

(defn bind [f-out g]
  (let [[fx fs] f-out
        [gx gs] (g fx)]
    [gx (clojure.string/join ", " [fs gs])]))

(g (f 2))
; => 5

(bind (f' 2) g')
; => [5 "times two was called, plus one was called"]


; Now let's get rid of f' g' definitions

(defn return [x]
  [x ""])


; Lift takes an ordinary function, its debug string and makes it into a debuggable
; function. Notice that we haven't touched implementations of f,g at all.

(defn lift [f debug-str]
  (fn [x] [(f x) debug-str]))

(let [f' (lift f "f called")
      g' (lift g "g called")]
  (bind (f' 2)
        g'))
; => [5 "f called, g called"]


; This way we "decorated" functions f and g with a debug string and we defined a way
; how to chain/combine the decorated functions using "bind".
