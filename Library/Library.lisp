(
(def defmacro (macro (name args body) (def name (macro args body))))
;@\[name,args,body] -> (def name (macro args body));

(def defn (macro (name args body) (def name (lambda args body))))
; @\[name,args,body] -> (def name (lambda args body));

(defmacro defnn (name args body) (def name (lambda args body)))
; @\[name,args,body] -> (def name (lambda args body));

; (defmacro defnn_ (name_ args_ body_) (def name_ (lambda args_ body_))) для отладки было;
; @\[name_,args_,body_] -> (def name_ (lambda args_ body_));


; (def nil '());

; (defn aa (x) (+ x x));

; (defn2 aa2 (x) (+ 1 x));

(defn caar args (car (cdr args)))

(defmacro caarm args (car (cdr args)))

)
