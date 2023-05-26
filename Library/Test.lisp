;(macro (x y z) ('(x y z))) a b ( c d); 
; этот макрос работает = macroexpand (('( a b (c d))));
(
((macro (x y) ( (macro (x) (+ x y)) 1 ) ) 100 (+ 1 20))
;22;

((macro (x y) ( (lambda (x) (+ x y)) 2 ) ) 100 (+ 10 2))
;14;

; (macro name ( (lambda name (+ x y)) 2 3) ) (x y);

; (macro name ( (macro name (+ x y)) 2 3) ) (x y);

(def m  ( macro (name) ((lambda name   (+ x y)) 2 3)))
(def m1 ( macro name   ((lambda name   (+ x y)) 2 3)))
(def m2 ( macro name   ((lambda (name) (+ x y)) 2 3)))
(def m3 ( macro (name) ((lambda (name) (+ x y)) 2 3)))
( +
  11
  12
)
)

; ((macro (x y z) '((x y z))) a b (c d));


; ((macro (x y) ( (macro (x) (+ x y)) 1) ) a b) ;
;(macro (x y z) ( (macro (z) '(+ x y z)) '1 '2 ) )  a b (c d);
;(macro (x y z) ( (macro (z) (+ x y a)) 1 ) ) 10 2 a ;

;(def defn (macro (name args body) (def name (lambda args body))) );
; (def defn (macro (name args body) (def name (lambda args body))) ) eto rabotaez, kogda zalazim v args lambdu;
