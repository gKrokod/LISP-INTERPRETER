; make Test for defmacro ;
(
(def defmacro (macro (name args body) (def name (macro args body))))
;@\[name,args,body] -> (def name (macro args body));
; DEFMACRO name args body;

; (def defn (macro (name args body) (def name (lambda args body))));
; @\[name,args,body] -> (def name (lambda args body));

(defmacro defun (name args body) (def name (lambda args body)))
; @\[name,args,body] -> (def name (lambda args body));

; (defmacro defnn_ (name_ args_ body_) (def name_ (lambda args_ body_))) для отладки было;
; @\[name_,args_,body_] -> (def name_ (lambda args_ body_));


;------------------------------------------------- extension core;
; memory;
(def nil '())

(def void "{}") ; затычка не связанная с SExpre Void, но несущая тот же смысл ;
(defmacro null args (== args nil))

(defmacro isBool arg (== "Bool" (typeof arg)))
(defmacro isInt arg (== "Int" (typeof arg)))
(defmacro isDouble arg (== "Double" (typeof arg)))
(defmacro isNumber arg (cond ((isInt arg) #t)
                             ((isDouble arg) #t)
                             (#t #f))) 

(defmacro isString arg (== "String" (typeof arg)))
(defmacro isAtom arg (== "Atom" (typeof arg)))
(defmacro isList arg (== "List" (typeof arg)))

(defmacro not arg (cond (arg #f) 
                        ((isBool arg) #t) 
                        (#t (print "Exception not bool argument"))))) 
(defmacro if (predicate thenDo elseDo) (cond (predicate thenDo) 
                                             ((isBool predicate) elseDo) 
                                             (#t (print "Exception not bool argument") ))) ; eq $ bool p then else;
(defmacro setq (atom value) (set 'atom value))

;--Logic family;
(defmacro and (x y) (if (isBool x)
                       (if (isBool y) 
                          (cond (x y) (#t #f) ) ((print "Exception not bool second argument") void) ) 
                       ((print "Exception not bool argument") void) ))
(defmacro or (x y) (if (isBool x)
                       (if (isBool y) 
                          (cond (x #t) (#t y) ) ((print "Exception not bool second argument") void) ) 
                       ((print "Exception not bool argument") void) ))
(defmacro xor (x y) (and (or x y) (not (and x y))))

(defmacro greaterp (x y) (cond ((== (typeof x) (typeof y)) (> x y))
                               (#t ((print "Exception different types on arguments") void))))
(defmacro greqp (x y) (cond ((== (typeof x) (typeof y)) (>= x y))
                            (#t ((print "Exception different types on arguments") void))))
(defmacro lessp (x y) (cond ((== (typeof x) (typeof y)) (< x y))
                            (#t ((print "Exception different types on arguments") void))))
(defmacro leeqp (x y) (cond ((== (typeof x) (typeof y)) (<= x y))
                            (#t ((print "Exception different types on arguments") void))))
(defmacro eq (x y) (cond ((== (typeof x) (typeof y))  (== x y))
                         (#t ((print "Exception different types on arguments") void))))
(defmacro neq (x y) (cond ((== (typeof x) (typeof y)) (not (== x y)))
                          (#t ((print "Exception different types on arguments") void))))

;--Logic Arithmetic;
(defun abs x (if (> x 0) x (* (-1) x))) 
(defun max (x y) (if (> x y) x y)) 
(defun min (x y) (if (< x y) x y)) 
(defun signum x (cond ((not (isNumber x)) (print "Exception: not a Number")) 
                      ((> x 0) 1)
                      ((< x 0) (-1))
                      ((== x 0) 0)))
; id, map ;
(defun id x x)
(defun map (f xs) (cond ((null xs) nil)
                       (#t (cons (f (car xs)) (pf (cdr xs) f)))))

; пример расчета чисел фибоначчи ;
(defun fib x 
  (cond ((== 0 x) 0)
        ((== 1 x) 1)
        (#t (+ (fib (- x 1))
               (fib (- x 2))))))

; пример передачи функции в качестве аргумента;
(defun x2 (x) (^ x 2))
(defun pf (xs f) (cond ((null xs) nil)
                       (#t (cons (f (car xs)) (pf (cdr xs) f)))))



;-------------------- CAR and CDR family ;
(defmacro cdar args (cdr (car args)))
(defmacro caar args (car (car args)))
(defmacro cadr args (car (cdr args)))
(defmacro cddr args (cdr (cdr args)))
(defmacro cdddr args (cdr (cdr (cdr args))))
(defmacro cadar args (car (cdr (car args))))
(defmacro caddr args (car (cdr (cdr args))))
(defmacro cadddr args (car (cdr (cdr (cdr args)))))
;----------------------------------------;
; base function;
(defun length args (cond ((null args) 0) (#t (+ 1 (length (cdr args))))))
(defun sum-list args (cond ((null args) 0) (#t (+ (car args) (sum-list (cdr args))))))

;(def length (lambda args (cond ((null args) 0) (#t (+ 1 (length (cdr args)))))));
;(def sum-list (lambda args (cond ((null args) 0) (#t (+ (car args) (sum-list (cdr args)))))));

;(defun len args ((cond ((null args) 0) (#t (+ 1 (len (cdr args)))))));

)
; ---------------------------------------- описание функций;
; null list - empty list? ;
; set a b - присвоить атому а, значение б. Вычисляет оба аргумента - core;
; setq a b - вычисляет только аргумент b;
; cond (condition1 result1) (condition2 result2) .. (conditionN resultN) - core;
; if condition  resultIfTrue resultIfFalse - eval all, if condition :: Bool then IF else PRINT Excpetion;
; not arg - eval arg, if arg :: Bool then NOT else PRINT Exception;
; car xs = head xs;
; cdr xs = tail xs;
; cons a b = (eval a) : (eval b);
; lenght xs = ;
;  ex: lenght '(1 2 30) = 3;
; sum-list xs = ;
;  ex: sum-list '(1 2 30) = 33;
; defun name args body - функция с именем name, списком формальных параметров args и телом body;
; \div = div ;
; \mod = mod ;
; \ = \ ;
; ^ = ^ or **;
; + = + or ++;




