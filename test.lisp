1 ;; with (tricky comments
;; comments work with # and ;
(+ (- (* (/ 1 2) 3) 4) 5 6 7)
(+ (- (* (/ 0b1 2) 3) 4) 5 0x6 007 0.0)
(ruby "puts \"\\\"quoted escaped\\\"\"")
(defvar x 1)
(defvar y 'puts "y"')
(ruby y)
(puts "ok")

(defun0 f1 (puts "fun1"))
(f1)

(defun f2 ()
  (puts "fun2"))
(f2)
;; with argument
(defun g (garg)
  (puts garg))
(g "value")
;; argument list is not available outside
;;(puts garg)

;; but setq is global
(defun g2 (garg)
  (let garg2 garg))
(g2 "test")
;; not in this scope
;;(puts garg)
;;(puts garg2)

(progn (puts "1") (puts "2"))
;; now does not resolve (nil => 0) and throws error
;;(if (not (= garg 0)) (puts "true BAD ANSWER") (puts "false"))
;; not in this scope, declare new
(defvar garg2 "test2")
(if garg2
    (puts "true")
    (puts "false BAD ANSWER"))
;; this should error
;;garg
;; this is ok
garg2
(boundp garg2)
;; does not throw
(boundp garg)
(if (not (boundp garg))
    (puts "true")
    (puts "false BAD ANSWER"))
((lambda (x) (puts x))
 "invoke lambda")
((defun named (x) (puts x))
 "invoke defun")
(puts "\ttest\n")
;; declared from code
(internal)
(list 1 2 3)

(+ 2)
(- 2)
(* 2)
(/ 2)
(+)
(*)
;; not allowed without argument
;(-)
;(/)
(reduce + (list 1 2 3))
(map (lambda (x) (* x x)) (list 1 2 3))
(map g (list 1 2 3))

(defvar // (lambda (x) (puts x)))
;; use defvar
(funcall // 3)
(funcall (lambda (x) (puts "x"))
         3)
;; use defun
;; (function g)
(funcall (function g) 4)

(funcall (function (lambda (x) (puts "fn lambda")))
         4)

(defvar //2 (lambda (a b c) (puts a b c)))
(apply //2 1 (list 2 3))
(apply (function /)
       1 (list 2 3))

;; let works in current context - new context created by defun/defun1/lambda
;; can't access higher levels, only global context
(defvar var "global")
;; global
(progn 
  (puts var)
  ((lambda ()
     (progn
       (print "1st lambda: " var "\n")
       (setq var "global2")
       (print "after setq: " var "\n")
       (let var "local ")
       (print "after let: " var "\n")
       ((lambda () (progn
                     ;; global2 again without tracking call stack
                     ;; now local
                     (print "2nd lambda: " var "\n")
                     (puts "backtrace" (backtrace))))
       ))))
  ;; global2
  (print "outside: " var "\n")
  )

;; list reimplemented with cons, cons-based data structures are possible now
(cons 1 2)
(cons 1 (cons 2 nil))
(cons 1 (cons 2 3))

(mod 1 2)
(defun oddp (x)
  (= 1 (mod x 2)))
(defun evenp (x)
  (= 0 (mod x 2)))
(oddp 2)
(evenp 2)

(defvar x 1)
(defvar l
  ((lambda () (progn
                ;; progn doesn't change scope, lambda does
                (print "\tglobal " (= x 1) "\n")
                (let x 2)
                (print "\tlocal " (= x 2) "\n")
                ((lambda () (progn
                              (print "\tlambda " (= x 2) "\n")
                              (let x 3)
                              (print "\tlambda " (= x 3) "\n")
                              )))
                (print "\tlocal " (= x 2) "\n")
                ;; value for defvar
                (lambda () x)
                ))))
(print "\tglobal " (= x 1) "\n")
;; this uses x from this context, it is not saved, it would be 2
(print "\tlambda binds to global: " (= 1 (funcall l)) "\n")

(defun filter-list (keep list)
  (progn
    (defvar result (list))
    (map (lambda (x)
           (progn
             ;(print "mapping" x (funcall keep x) "\n")
             (if (funcall keep x)
                 ;; prepending
                 ;; TODO because empty list is not nil this leaves (nil . nil)
                 ;;      at the end
                 (setq result (cons x result))
                 ;; else
                 (puts result)
                )))
         list)
    result))
(filter-list (function oddp) (list 1 2 3 4 5))

(exit 0)
;; end
