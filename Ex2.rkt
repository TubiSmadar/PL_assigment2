;;Q_1
(: square : Number -> Number) ;Definition
(define (square num) ;Helper function that gets a number and returns the square of that number
  (* num num))


(: sum-of-squares : (Listof Number) -> Number) ;Definition
(define (sum-of-squares lst)
  (foldl + 0 (map square lst))) ;Returns the sum of the squares of the numbers in lst

;; Tests for Q_1
(test (sum-of-squares '(1 2 3)) => 14) ;Base case
(test (sum-of-squares '(1)) => 1) ; Square of 1 case
(test (sum-of-squares '()) => 0) ;Empty list case 
(test (sum-of-squares '(0 0 0 0)) => 0) ;Square of 0's case
(test (sum-of-squares '(1 2 3 4 5 6 7 8 9 10)) => 385) ;Another basic test
(test (sum-of-squares '(1 2 3 4 5 6 7 8 10 9)) => 385) ;Out of order case

;;Q_2.a
(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
 (: poly : (Listof Number) Number Integer Number -> Number)
 (define (poly argsL x power accum)
 (if (null? argsL)
     accum
     (poly (rest argsL) x (+ power 1)
           (+ accum (* (first argsL) (expt x power))))))
  
 (: polyX : Number -> Number)
 (define (polyX x)
  (poly coeffs x 0 0))

polyX)


;; Tests for Q_2.a
(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)
(define p2345 (createPolynomial '(2 3 4 5))) 
(test (p2345 0) => (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3)))) 
(test (p2345 4) => (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3)))) 
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3)))) 
(define p536 (createPolynomial '(5 3 6))) 
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2)))) 


;;Q_2.b.1
#|
 The grammar:
 <PLANG> ::=
    {{Symbol <AEs>} {<AEs>}}
 <AEs> ::=
     <AE>
    |{<AE> <AEs>}
 <AE> ::=
     <num>
    |{+ <AE> <AE>}
    |{- <AE> <AE>}
    |{* <AE> <AE>}
    |{/ <AE> <AE>}
 |#

;;Q_2.b.2
(define-type PLANG
  [Poly (Listof AE) (Listof AE)]) 

(define-type AE
 [Num Number]
 [Add AE AE]
 [Sub AE AE]
 [Mul AE AE]
 [Div AE AE])

 (: parse-sexpr : Sexpr -> AE)
 ;; to convert s-expressions into AEs
 (define (parse-sexpr sexpr)
   (match sexpr
     [(number: n) (Num n)]
     [(list '+ lhs rhs) (Add (parse-sexpr lhs)
                             (parse-sexpr rhs))]
     [(list '- lhs rhs) (Sub (parse-sexpr lhs)
                             (parse-sexpr rhs))]
     [(list '* lhs rhs) (Mul (parse-sexpr lhs)
                             (parse-sexpr rhs))]
     [(list '/ lhs rhs) (Div (parse-sexpr lhs) 
                             (parse-sexpr rhs))]
   [else (error 'parse-sexpr "bad syntax in ~s"
                sexpr)])) 

 (: parse : String -> PLANG)
 ;; parses a string containing a PLANG expression to a PLANG AST
 (define (parse str)
   (let ([code (string->sexpr str)])
     (match code
       [(list l r)
        (match l
          ['() (error 'parse "at least one coefficient is required in ~s " code)]
          [(list 'poly) (error 'parse "at least one coefficient is required in ~s " code)]
          [(list 'poly argsl ...) 
           (match r
             ['() (error 'parse "at least one point is required in ~s" code)]
             [(list argsr ...) (Poly (map parse-sexpr argsl) (map parse-sexpr argsr))])]
          [else (error 'parse "Must start with symbol 'poly in ~s" code)])]
       [else (error 'parse "bad syntax in ~s" code)])))
 
 ;;tests Q_2.b.2
(test (parse "{{poly 1 2 3} {1 2 3}}")  => (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 1) (Num 2) (Num 3))))  ;Base case
(test (parse "{{poly } {1 2} }") =error> "parse: at least one coefficient is required in ((poly) (1 2))") ;Error no coefficient case
(test (parse "{{poly 1 2} {} }") =error> "parse: at least one point is required in ((poly 1 2) ())") ;Error no point case
(test (parse "{{1 2} {} }") =error> "parse: Must start with symbol 'poly in ((1 2) ())") ;Error no 'poly case
(test (parse "{{1 2} {poly} }") =error> "parse: Must start with symbol 'poly in ((1 2) (poly))") ;Error start with point case
(test (parse "{{poly 1 2} {poly} }") =error> "parse-sexpr: bad syntax in poly") ;Error parsing case
(test (parse "{{poly 1 2} {poly 1} }") =error> "parse-sexpr: bad syntax in poly") ;Error parsing 2 case
(test (parse "{{1 2} {} {}}") =error> "parse: bad syntax in ((1 2) () ())") ;Error bad syntax case
(test (parse "{}") =error> "parse: bad syntax in ()") ;Error bad syntax 2 case
(test (parse "{{poly 1 2 3} {1 2 3} {4 5 6}}") =error> "parse: bad syntax in ((poly 1 2 3) (1 2 3) (4 5 6))") ;Error more than 1 point set
(test (parse "{{} {1 2} }") =error> "parse: at least one coefficient is required in (() (1 2))") 
(test (parse "{{poly 4/5 } {1/2 2/3 3}}") => (Poly (list (Num 4/5)) (list (Num 1/2) (Num 2/3) (Num 3)))) ;Base parse fractions case
(test (parse "{{poly {/ 4 2} {- 4 4}} {{- 8 8}}}") => (Poly (list (Div (Num 4) (Num 2)) (Sub (Num 4) (Num 4))) (list (Sub (Num 8) (Num 8))))) ;Base case with operators


;;Q_2.b.3
;; evaluates AE expressions to numbers
(: eval : AE -> Number)
 (define (eval expr)
   (cases expr
     [(Num n) n]
     [(Add l r) (+ (eval l) (eval r))]
     [(Sub l r) (- (eval l) (eval r))]
     [(Mul l r) (* (eval l) (eval r))]
     [(Div l r) (/ (eval l) (eval r))]))


 (: eval-poly : PLANG -> (Listof Number))
 (define (eval-poly p-expr)
   (: eval-poly-helper : (Listof AE) (Listof Number) -> (Listof Number))
   (define (eval-poly-helper lst current)
     (cond
       [(null? lst) current]
      [else (eval-poly-helper (rest lst) (append current (list (eval (first lst)))))]))

   (cases p-expr
     [(Poly l r)
      (map (createPolynomial (eval-poly-helper l '())) (eval-poly-helper r '()))]))
    

 (: run : String -> (Listof Number))
 ;; evaluate a FLANG program contained in a string
 (define (run str)
   (eval-poly (parse str)))

;;tests Q_2.b.3
(test (run "{{poly 1 2 3} {1 2 3}}")=> '(6 17 34)) ;Base case
(test (run "{{poly 4 2 7} {1 4 9}}") =>'(13 124 589)) ;Base case 2
(test (run "{{poly 4/5 } {1/2 2/3 3}}")=> '(4/5 4/5 4/5)) ;Fraction case
(test (run "{{poly 2 3} {4}}") => '(14)) ;One item case
(test (run "{{poly 1 1 0} {-1 3 3}}") => '(0 4 4)) ;Negative item case
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") => '(14)) ;Division and substitution evaluation case
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => '(0 4 4)) ;Complicated case
