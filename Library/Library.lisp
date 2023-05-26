; make Test for defmacro ;
(
(def defmacro (macro (name args body) (def name (macro args body))))
;@\[name,args,body] -> (def name (macro args body));
; DEFMACRO name args body;

; (def defn (macro (name args body) (def name (lambda args body))));
; @\[name,args,body] -> (def name (lambda args body));

(defmacro defn (name args body) (def name (lambda args body)))
; @\[name,args,body] -> (def name (lambda args body));

; (defmacro defnn_ (name_ args_ body_) (def name_ (lambda args_ body_))) для отладки было;
; @\[name_,args_,body_] -> (def name_ (lambda args_ body_));


;------------------------------------------------- extension core;
(def nil '())
(defmacro not arg (cond (arg #f) ((== (typeof arg) "Bool") #t) (#t (print "Exception not bool argument"))))) 
(defmacro if (predicate thenDo elseDo) (cond (predicate thenDo) ((== (typeof arg) "Bool") elseDo) (#t (print "Exception not bool argument") ))) ; eq $ bool p then else;
(defmacro setq (atom value) (set 'atom value))

;-------------------- CAR and CDR family ;
(defmacro cdar args (cdr (car args)))
(defmacro caar args (car (car args)))
(defmacro cadr args (car (cdr args)))
(defmacro cddr args (cdr (cdr args)))
(defmacro cdddr args (cdr (cdr (cdr args))))
(defmacro cadar args (car (cdr (car args))))
(defmacro caddr args (car (cdr (cdr args))))
(defmacro cadddr args (car (cdr (cdr (cdr args)))))
;----------------------------------------

)
; ---------------------------------------- описание функций;
; set a b - присвоить атому а, значение б. Вычисляет оба аргумента - core;
; setq a b - вычисляет только аргумент b;
; cond (condition1 result1) (condition2 result2) .. (conditionN resultN) - core;
; if condition  resultIfTrue resultIfFalse - eval all, if condition :: Bool then IF else PRINT Excpetion;
; not arg - eval arg, if arg :: Bool then NOT else PRINT Exception;
; car xs = head xs;
; cdr xs = tail xs;
; cons a b = (eval a) : (eval b);


