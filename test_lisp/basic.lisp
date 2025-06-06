(SET 'COUNTER 0)
(SET 'ASSERT
    (LAMBDA (X) (IF (EQ NIL (EVAL X))
                    ((LAMBDA (X) (PRINT COUNTER) (PRINT X)) X)
                    (PROGN
                      (PRINT X)
                      (SET 'COUNTER (ADD COUNTER 1))))
    )
)
;; basic expressions
(ASSERT 'T)
(ASSERT '(NOT NIL))
(ASSERT '(EQ NIL '()))
(ASSERT '(NOT '()))
(ASSERT '42)
(ASSERT ''FOO)
(ASSERT ''(1 2 3))
;; EQ
(ASSERT '(EQ T T))
(ASSERT '(EQ NIL NIL))
(ASSERT '(NOT (EQ T NIL)))
(ASSERT '(NOT (EQ NIL T)))
(ASSERT '(EQ T T))
(ASSERT '(EQ 42 42))
(ASSERT '(NOT(EQ 42 0)))
(ASSERT '(EQ 'FOO 'FOO))
(ASSERT '(NOT(EQ 'FOO 'BAR)))
;; symbols
(ASSERT '(SET 'FOO 'BAR))
(ASSERT ''FOO)
(ASSERT '(SET 'FOO '(1 2 3)))
(ASSERT ''FOO)
;; CONS
(ASSERT '(CONS 'FOO 'BAR))
;; FIRST
(ASSERT '(NOT (FIRST '())))
(ASSERT '(NOT (CAR (FIRST '()))))
(ASSERT '(FIRST '(T)))
(ASSERT '(EQ 'BAR (FIRST '(BAR))))
;; REST
(ASSERT '(NOT (REST '())))
(ASSERT '(EQ NIL (REST NIL)))
(ASSERT '(EQ NIL (CDR (REST '()))))
;; LIST
(ASSERT '(EQ NIL (LIST)))
(ASSERT '(LENGTH (LIST 'FOO 'BAR)))
;; LENGTH
(ASSERT '(EQ (LENGTH NIL) 0))
(ASSERT '(EQ (LENGTH NIL) (LENGTH NIL)))
(ASSERT '(NOT (EQ (LENGTH '(1)) (LENGTH NIL))))
;; ;; PAIR
;; (ASSERT '(EQ 0 (LENGTH (PAIR '() '()))))
;; (ASSERT '(EQ 1 (LENGTH (PAIR '(1) '(1)))))
;; (ASSERT '(EQ 2 (LENGTH (PAIR '(A B) '(1 2)))))
;; IF
(ASSERT '(IF T T))
(ASSERT '(NOT (IF T NIL)))
(ASSERT '(IF T T NIL))
(ASSERT '(IF NIL NIL T))
;; LAMBDA
(ASSERT '(LAMBDA () ()))
(ASSERT '(LAMBDA (X) T))
(ASSERT '((LAMBDA (X) T) 42))
(ASSERT '(EQ 42 ((LAMBDA (X) X) 42)))
(SET 'FOOBAR 42)
(ASSERT '(EQ 42 ((LAMBDA () FOOBAR))))
;; APPLY
(ASSERT '(APPLY FIRST '((T))))
(ASSERT '(NOT (APPLY REST '((T)))))
(ASSERT
    '(APPLY CONS '(FOO 'BAR)))
(ASSERT
    '(EQ 42 (APPLY FUNCALL '(CAR (42 0)))
    ))
;; FUNCALL
(ASSERT
    '(FUNCALL (LAMBDA () T)))
(ASSERT
    '(FUNCALL (LAMBDA (X Y) (CONS X Y)) 'FOO 'BAR))
;; MATH (very buggy due to defaults)
;; BUGS
;; (ASSERT '(EQ 0 (+)))
;; (ASSERT '(EQ 1 (*)))
(ASSERT '(EQ 42 (+ 42)))
;; BUGS
;; (ASSERT '(EQ -42 (- 42))) == -42
(ASSERT '(EQ 42 (* 42)))
;; BUG SHOULD NOT WORK
;; (ASSERT '(EQ 42 (/ 42)))
(ASSERT '(EQ 10 (+ 1 2 3 4)))
(ASSERT '(EQ -8 (- 1 2 3 4)))
(ASSERT '(EQ 42 (* 21 2)))
(ASSERT '(EQ 42 (/ 84 2)))
;; BOOLEANS
(ASSERT '(GT 42 -42))
(ASSERT '(GT 0 -1 -2 -3))
(ASSERT '(LT -42 42))
(ASSERT '(LT -1 0 1 2 3))
;; RECURION
(SET 'LESS_THAN_OR_EQ (LAMBDA (X Y) (OR (< X Y) (EQ X Y))))
(SET 'FOOER (LAMBDA (X)
    (IF (LESS_THAN_OR_EQ X 0)
        X
        (FOOER (SUB X 1))
    )))
(ASSERT (EQ 0 (FOOER 42)))
(ASSERT (EQ -42 (FOOER -42)))
(SET 'RANGE (LAMBDA (X MAX)
    (IF (> X MAX)
        NIL
        (CONS X (RANGE (ADD X 1) MAX))
    )))
(ASSERT '(EQ 10 (LENGTH (RANGE 0 9))))
(ASSERT '(EQ 42 (LENGTH (RANGE 0 41))))

(ASSERT '(EQ 903 (APPLY + (RANGE 0 42))))
;; Fibonacci
(SET 'FIB (LAMBDA (X)
            (IF (OR (< X 1) (EQ X 1))
                X
                (+ (FIB (SUB X 1)) (FIB (SUB X 2)))
            )))
(ASSERT '(EQ 0 (FIB 0)))
(ASSERT '(EQ 1 (FIB 1)))
(ASSERT '(EQ 1 (FIB 2)))
(ASSERT '(EQ 2 (FIB 3)))
(ASSERT '(EQ 2 (FIB 3)))
(ASSERT '(EQ 5 (FIB 5)))
(ASSERT '(EQ 55 (FIB 10)))
;; Delayed evaluation Fibonacci
(SET 'LAYZ_FIB (LAMBDA (X)
            (IF (OR (< X 1) (EQ X 1))
                (LIST '+ X 0)
                (LIST '+ (FUNCALL LAYZ_FIB (SUB X 1)) (FUNCALL LAYZ_FIB (SUB X 2)))
            )))
(ASSERT '(EQ 0 (EVAL (LAYZ_FIB 0))))
(ASSERT '(EQ 1 (EVAL (LAYZ_FIB 1))))
(ASSERT '(EQ 1 (EVAL (LAYZ_FIB 2))))
(ASSERT '(EQ 2 (EVAL (LAYZ_FIB 3))))
(ASSERT '(EQ 2 (EVAL (LAYZ_FIB 3))))
(ASSERT '(EQ 5 (EVAL (LAYZ_FIB 5))))
(ASSERT '(EQ 55 (EVAL (LAYZ_FIB 10))))
(SET 'RESULT 6765)
(ASSERT '(EQ RESULT (EVAL (LAYZ_FIB 20))))
(SET 'RESULT 832040)
(ASSERT '(EQ RESULT (EVAL (LAYZ_FIB 30)))) ;; 32 is the max as of 5-17-25
(PRINT 'END)
(ASSERT NIL)