; make Test for defmacro ;
; Реализуй reverse, map cherez go for tail optimisation i sravni, [1..n], foldr, ;
; foldl f xs ini = foldr (\g x b -> g $ f x b) id xs ini ;
(
(def defmacro (macro (name args body) (def name (macro args body))))
;@\[name,args,body] -> (def name (macro args body));
; DEFMACRO name args body;

; (def defn (macro (name args body) (def name (lambda args body))));
; @\[name,args,body] -> (def name (lambda args body));

(defmacro defun (name args body) (def name (lambda args body)))
(defmacro defn (name args body) (def name (lambda args body)))
;(defmacro define (name body) (def name body));
; @\[name,args,body] -> (def name (lambda args body));

; (defmacro defnn_ (name_ args_ body_) (def name_ (lambda args_ body_))) для отладки было;
; @\[name_,args_,body_] -> (def name_ (lambda args_ body_));
; def = define; 
; defun = defn;
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
(defun sqr (x) (^ x 2))
(defun sqrt (x) (^ x 0.5))
(defun add (x y) (+ x y))
(defun sub (x y) (- x y))

;-- Usefull function id, flip;
(defun id x x)
(defun flip f (lambda (x y) (f y x)))

;-- List function: map, filter, foldr, foldl, enum, length, sum-list, take, drop;
(defun enum (start end) (
        (defun go (i result) (cond  ((> i end) nil) 
                                    ((== i end) (cons i result))
                                    (#t (go (+ i 1) (cons i result)))))
        (reverse (go start nil))))

(defun map (f xs) (
        (defun go (xs result) (cond ((null xs) result)
                                    (#t (go (cdr xs) (cons (f (car xs)) result)))))
        (reverse (go xs nil))))

(defun filter (p xs) (
        (defun go (xs result) (cond ((null xs) result)
                                    (#t (go (cdr xs) (if (p (car xs)) (cons (car xs) result) result)))))
        (reverse (go xs nil))))

(defun foldr (f acc xs)
  (cond ((null xs) acc)
        (#t (f (car xs) (foldr f acc (cdr xs))))))

; ghci> foldl2r f ini xs = foldr (\x g y -> g $ f y x) id xs in;
(defun foldl (f ini xs)
  ((foldr (lambda (x g) 
            (lambda y (g (f y x))))
            id
            xs)
   ini))

(defun reverse xs 
  (foldl (flip 'cons) nil xs))

(defun length args (cond ((null args) 0) (#t (+ 1 (length (cdr args))))))
(defun sum-list args (cond ((null args) 0) (#t (+ (car args) (sum-list (cdr args))))))

(defun take (num xs) (
  (defun go (num xs acc) ( cond ((null xs) acc)
                                ((== num 0) acc)
                                (#t (go (- num 1) (cdr xs) (cons (car xs) acc)))))
  (reverse (go num xs nil))))

(defun drop (num xs) (
  cond ((null xs) nil)
       ((== num 0) xs)
       (#t (drop (- num 1) (cdr xs)))))

; ghci> foldl (-) 10 [1..10] = -45;
; ghci> foldr (-) 10 [1..10] = 5;

; пример расчета чисел фибоначчи ;
(defun fib x 
  (cond ((== 0 x) 0)
        ((== 1 x) 1)
        (#t (+ (fib (- x 1))
               (fib (- x 2))))))

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
; div = div ;
; mod = mod ;
; \ = \ ;
; ^ = ^ or **;
; + = + or ++;
; enum 1 10 = (1,1,2,3,4,5,6,7,8,9,10);
; map f xs = map;
; id = id;
; flip = flip;
; reverse = reverse;
