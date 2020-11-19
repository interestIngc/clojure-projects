(defn operate [op] (fn [& args] (fn [vars] (apply op (mapv #(% vars) args)))))


(def add (operate +))
(def subtract (operate -))
(def multiply (operate *))
(defn do-divide [& xs] (if (= (count xs) 1)
                            (/ 1.0 (double (first xs)))
                            (apply (fn [a b] (/ (double a) b)) xs)))
(def divide (operate do-divide))
(def negate (operate -))

(defn constant [x] (fn [_] x))
(defn variable [arg] (fn [vars] (get vars arg)))
(defn do-avg [& xs] (/ (apply + xs) (count xs)))
(def avg (operate do-avg))

(def med (operate (fn [& args] (nth (sort args) (quot (count args) 2)))))

(def operations
  {'+ add
   '* multiply
   '- subtract
   '/ divide
   'negate negate
   'avg avg
   'med med})


(defn toParse [string operations constant variable]
  (letfn [(parse [expr] (cond
                          (list? expr) (apply (get operations (first expr)) (mapv parse (rest expr)))
                          (symbol? expr) (variable (str expr))
                          (number? expr) (constant expr)))]
    (parse (read-string string))))
(defn parseFunction [string] (toParse string operations constant variable))


;;                      ------------------------------
;;                      ------------------------------
(defn proto-get [obj key] (cond (contains? obj key) (obj key)
                            (contains? obj :prototype) (proto-get (obj :prototype) key)
                            :else nil))

(defn proto-call [this key & args] (apply (proto-get this key) this args))
(defn field [key]
  (fn [this] (proto-get this key)))
(defn method [key]
  (fn [this & args] (apply proto-call this key args)))
(defn constr [cons proto]
  (fn [& args] (apply cons {:prototype proto} args)))
(def evaluate (method :evaluate))
(def toString (method :toString))
(def diff (method :diff))
(def toStringInfix (method :toStringInfix))
(declare Zero)
(def Constant (let [_val (field :val)]
                (constr (fn [this val] (assoc this :val val))
                        {:evaluate (fn [this vars] (_val this))
                        :toString (fn [this] (format "%.1f" (_val this)))
                        :diff (fn [this name] Zero)
                        :toStringInfix (fn [this] (format "%.1f" (_val this)))})))
(def Zero (Constant 0))
(def One (Constant 1))
(def Variable (let [_var (field :var)]
                (constr (fn [this var] (assoc this :var var))
                        {:evaluate (fn [this vars] (get vars (_var this)))
                         :toString _var
                         :diff (fn [this name] (if (= name (_var this)) One Zero))
                         :toStringInfix _var})))

(def Operation-proto (let [_func (field :func)
                           _operate (field :operate)
                           _diffRule (field :diffRule)
                           _args (field :args)]
                       {:evaluate (fn [this vars] (apply (_func this) (mapv #(evaluate % vars) (_args this))))
                       :toString (fn [this] (str "(" (_operate this) " " (clojure.string/join " " (mapv toString (_args this))) ")"))
                       :diff (fn [this var] ((_diffRule this) (_args this) (mapv #(diff % var) (_args this))))
                        :toStringInfix (fn [this] (if (= 1 (count (_args this)))
                                                    (str (_operate this) "(" (toStringInfix (first (_args this))) ")")
                                                    (str "(" (clojure.string/join (str " "(_operate this) " ")
                                                                                  (mapv toStringInfix (_args this))) ")")))}))


(defn make-operation [func operate diffRule]
  (constr (fn [this & args] (assoc this :args (vec args)))
          {:prototype Operation-proto
         :func func
         :operate operate
         :diffRule diffRule}))

(def Add (make-operation + "+" (fn [args d_args] (apply Add d_args))))
(def Subtract (make-operation - "-" (fn [args d_args] (apply Subtract d_args))))
(def Negate (make-operation - "negate" (fn [args d_args] (Negate (first d_args)))))
(declare Multiply)
(defn diff-multiply [args d_args] (second (reduce (fn [[a da] [b db]] [(Multiply a b)
                                                                       (Add (Multiply a db) (Multiply da b))])
                                                  (mapv vector args d_args))))
(def Multiply (make-operation * "*" diff-multiply))
(declare Divide)
(defn diff-divide [[arg & args] [d_arg & d_args]]
  (let [rest (apply Multiply args)
       diff-rest (diff-multiply args d_args)]
    (if (== 0 (count rest))
      (Negate (Divide One (Multiply arg arg)))
      (Divide (Subtract (Multiply d_arg rest) (Multiply arg diff-rest)) (Multiply rest rest)))))

(def Divide (make-operation do-divide "/" diff-divide))
(def Sum (make-operation + "sum" (fn [args d_args] (apply Sum d_args))))
(def Avg (make-operation do-avg "avg" (fn [args d_args] (Divide (apply Sum d_args) (Constant (count d_args))))))

(defn apply_operation ([f a b] (f a b)) ([f a b & rest] (partial f a (apply apply_operation f b rest))))

(declare Log)
(declare Pow)
(def E (Constant Math/E))
(defn log-diff [[x dx] [y dy]] (Subtract
                                (Divide
                                 dy
                                 (Multiply y (Log E x)))
                                (Divide
                                 (Multiply dx (Log x y))
                                 (Multiply x (Log E x)))))

(defn pow-diff [[x dx] [y dy]]
  (Multiply (Pow x (Subtract y One)) (Add (Multiply y dx) (Multiply x (Multiply (Log E x) dy)))))
(defn get_op [f] (partial apply_operation f))
(def log (get_op (fn [x y] (/ (Math/log (Math/abs y)) (Math/log (Math/abs x))))))
(def pow (get_op (fn [x y] (Math/pow x y))))
(def Log (make-operation log "//"
                         (fn [args d_args] (second (reduce log-diff (map vector args d_args))))))
(def Pow (make-operation pow "**" (fn [args d_args] (second (reduce pow-diff (map vector args d_args))))))

(def object-operations
  {'+ Add
   '- Subtract
   '* Multiply
   '/ Divide
   'negate Negate
   'avg Avg
   'sum Sum
   (symbol "//") Log
   (symbol "**") Pow
   })
(defn parseObject [expr] (toParse expr object-operations Constant Variable))

; ------------------------------------------------------------------------
; ------------------------------------------------------------------------


(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)
(defn _show [result]
  (if (-valid? result) (str "-> " (pr-str (-value result)) " | " (pr-str (apply str (-tail result))))
    "!"))
(defn tabulate [parser inputs]
  (run! (fn [input] (printf "    %-10s %s\n" (pr-str input) (_show (parser input)))) inputs))
(defn _empty [value] (partial -return value))
(defn _char [p]
  (fn [[c & cs]]
    (if (and c (p c)) (-return c cs))))
(defn _map [f result]
  (if (-valid? result)
    (-return (f (-value result)) (-tail result))))
(defn _combine [f a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar)
        (_map (partial f (-value ar))
              ((force b) (-tail ar)))))))
(defn _either [a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar) ar ((force b) str)))))
(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))
(defn +char [chars] (_char (set chars)))
(defn +char-not [chars] (_char (comp not (set chars))))
(defn +map [f parser] (comp (partial _map f) parser))
(def +parser _parser)
(def +ignore (partial +map (constantly 'ignore)))
(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))
(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))
(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))
(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))
(defn +or [p & ps]
  (reduce _either p ps))
(defn +opt [p]
  (+or p (_empty nil)))
(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))
(defn +plus [p] (+seqf cons p (+star p)))
(def *space (+char " \t\n\r"))
(def *ws (+ignore (+star *space)))
(defn +str [p] (+map (partial apply str) p))
(def *digit (+char "0123456789"))
(def *all-chars (mapv char (range 32 128)))
(def *letter (+char (apply str (filter #(Character/isLetter %) *all-chars))))

(def *number (+seqf
              (fn [sign left dot right] (read-string (str sign (apply str left) dot (apply str right))))
              (+opt (+char "+-"))
              (+plus *digit)
              (+opt (+char "."))
              (+star *digit)))
(def *variable (+map Variable (+str (+plus *letter))))
(def *constant (+map (comp Constant double) *number))
(defn +operation [p] (+map (comp (partial get object-operations) symbol str) p))
(def *negate (+operation
               (+seqf str (+char "n") (+char "e") (+char "g") (+char "a") (+char "t") (+char "e"))))
(defn apply_unary [factor]
    (letfn [(rec [ops]
                 (if (== (count ops) 0) (second factor) ((first ops) (rec (rest ops)))))]
      (rec (first factor))))
(declare *bracket)
(def *compound-unary
  (+map apply_unary (+seq *ws (+star (+map first (+seq *negate *ws)))
                          (+or *constant *variable (delay *bracket)))))
(def left-op (fn [a [op b]] (op a b)))
(def right-op (fn [a [op b]] (op b a)))
(defn binary [fun] (fn [[x & rest]] (reduce fun x (partition 2 rest))))
(def left-binary (binary left-op))
(defn right-binary [x] ((binary right-op) (reverse x)))
(defn +seq-op [_apply *value *operate]
  (+map _apply (+map flatten (+seq *ws *value *ws (+star (+seq *operate *ws *value *ws))))))
(defn *get_left_binary_func [symbol] (+operation (+char symbol)))
(def *add (*get_left_binary_func "+"))
(def *subtract (*get_left_binary_func "-"))
(def *multiply (*get_left_binary_func "*"))
(def *divide (*get_left_binary_func "/"))
(defn *get_right_binary_func [symbols] (let [string (apply +seqf str (map (comp +char str) symbols))]
                                         (+operation string)))
(def *pow (*get_right_binary_func "**"))
(def *log (*get_right_binary_func "//"))
(defn *apply_left_op [& ps] (+seq-op left-binary
                                   (first ps) (apply +or (rest ps))))
(def *expr (*apply_left_op
            (*apply_left_op
             (+seq-op right-binary *compound-unary (+or *pow *log))
             *multiply *divide) *add *subtract))
(def *bracket (+seqn 1 (+char "(") *expr (+char ")")))
(def parseObjectInfix (+parser *expr))

