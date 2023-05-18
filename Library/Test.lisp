;(macro (x y z) ('(x y z))) a b ( c d); 
; этот макрос работает = macroexpand (('( a b (c d))));

(macro (x y) ( (macro (x) (+ x y)) 1 ) ) a b

;(macro (x y z) ( (macro (z) '(+ x y z)) '1 '2 ) )  a b (c d);

;(macro (x y z) ( (macro (z) (+ x y a)) 1 ) ) 10 2 a ;

; (def defn (macro (name args body) (def name (lambda args body)))) ;

