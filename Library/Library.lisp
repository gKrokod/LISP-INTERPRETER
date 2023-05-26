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


;------------------------------------------------- extension core
(def nil '())
(defmacro if (predicate thenDo elseDo) (cond (predicate thenDo) (#t elseDo))) ; eq $ bool p then else;
(defmacro setq (atom value) (set 'atom value))


; (defn aa (x) (+ x x));

; (defn2 aa2 (x) (+ 1 x));

(defn caar args (car (cdr args)))

(defmacro caarm args (car (cdr args)))

)
; ---------------------------------------- описание функций;
; set a b - присвоить атому а, значение б. Вычисляет оба аргумента - core;
; setq a b - вычисляет только аргумент b;

