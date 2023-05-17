(def plus2 (lambda x (+ 2 x)))

(def mplus2 (macro x (+ 2 x)))

(def defn (macro (name args body) (def name (lambda args body))))


(defn plus x (+ x 1))

; (def x 100) ;

; (def y 400) ;

; (def nil '()) ;

(def bb 30)

; (def x5 '((macro (x) (cond (((< x 5) #t) (#t #f)))) x)) ;

(def aa 20)
