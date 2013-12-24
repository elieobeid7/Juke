(defun juke (x &optional env)
  "evaluate the expression x in the environment env."
  (cond
    ((symbolp x) ($$svar x env))
    ((atom x) x)
    ((case (first x)
       (QUOTE  (second x))
       (BEGIN  (last1 (mapcar #'(lambda (y) (juke y env))
                              (rest x))))
       
       
       (SET!   ($$svar (second x) (juke (third x) env) env))
       (IF     (if (juke (second x) env)
                   (juke (third x) env)
                   (juke (fourth x) env)))
       (LAMBDA (let ((parms (second x))
                     (code (add? 'begin (rest2 x))))
                 #'(lambda (&rest args)
                     (juke code (extend-env parms args env)))))
       (t      ;; a procedure application
               (apply (juke (first x) env)
                      (mapcar #'(lambda (v) (juke v env))
                              (rest x))))))))

(defun $svar (var val env)
  "Set a variable to a value, in the given or global environment."
  (if (assoc var env)
      (setf (second (assoc var env)) val)
      (set-global-$svar var val))
  val)

(defun $$svar (var env)
  "Get the value of a variable, from the given or global environment."
    (if (assoc var env)
        (second (assoc var env))
        (get-global-var var)))

(defun set-global-$svar (var val)
  (setf (get var 'global-val) val))

(defun get-global-var (var)
  (let* ((default "unbound")
         (val (get var 'global-val default)))
    (if (eq val default)
        (error "Unbound  variable: ~a" var)
        val)))

(defun extend-env (vars vals env)
  "Add some variables and values to an environment."
  (nconc (mapcar #'list vars vals) env))

(defparameter *-procs*
  '(+ - * / = < > <= >= cons car cdr not append list read member
    (null? null) (eq? eq) (equal? equal) (eqv? eql)
    (write prin1) (display princ) (newline terpri)))

(defun init-juke ()
  "Initialize the  juke ide with some global variables."
  ;; Define  procedures as CL functions:
  (mapc #'init-proc *-procs*)
  ;; Define the boolean `constants'.  
  ;; BUt yoy can still do (set! x nil)
  (set-global-$svar t t)
  (set-global-$svar nil nil))

(defun init-proc (f)
  "Define a  procedure as a corresponding CL function."
  (if (listp f)
      (set-global-$svar (first f) (symbol-function (second f)))
      (set-global-$svar f (symbol-function f))))

(defun  (&optional x)
  "A  read-eval-print loop (using juke)"
  ;;  handle optional argument
  ;; instead of always going into a loop.
  (init-juke)
  (if x
      (juke x nil)
    (loop (format t "~&==> ")
      (print (juke (read) nil)))))



(defun juke (x &optional env)
 "(evaluate) the expression x in the environment env."
  (cond
    ((symbolp x) ($$svar x env))
    ((atom x) x)
    ((juke-macro (first x))              ;***
     (juke (juke-macro-expand x) env)) ;***
    ((case (first x)
       (QUOTE  (second x))
       (BEGIN  (last1 (mapcar #'(lambda (y) (juke y env))
                              (rest x))))
       (SET!   ($$svar (second x) (juke (third x) env) env))
       (IF     (if (juke (second x) env)
                   (juke (third x) env)
                   (juke (fourth x) env)))
       (LAMBDA (let ((parms (second x))
                     (code (add? 'begin (rest2 x))))
                 #'(lambda (&rest args)
                     (juke code (extend-env parms args env)))))
       (t      ;; a procedure application
               (apply (juke (first x) env)
                      (mapcar #'(lambda (v) (juke v env))
                              (rest x))))))))

;;; ==============================

(defun juke-macro (symbol)
  (and (symbolp symbol) (get symbol 'juke-macro)))

(defmacro def-juke-macro (name parmlist &body body)
  "Define a  macro."
  `(setf (get ',name 'juke-macro)
         #'(lambda ,parmlist .,body)))

(defun juke-macro-expand (x)
  "Macro-expand this  expression."
  (if (and (listp x) (juke-macro (first x)))
      (juke-macro-expand
        (apply (juke-macro (first x)) (rest x)))
      x))

;;; ==============================

(def-juke-macro let (bindings &rest body)
  `((lambda ,(mapcar #'first bindings) . ,body)
    .,(mapcar #'second bindings)))

(def-juke-macro let* (bindings &rest body)
  (if (null bindings)
      `(begin .,body)
      `(let (,(first bindings))
         (let* ,(rest bindings) . ,body))))

(def-juke-macro and (&rest args)
  (cond ((null args) 'T)
        ((length=1 args) (first args))
        (t `(if ,(first args)
                (and . ,(rest args))))))

(def-juke-macro or (&rest args)
  (cond ((null args) 'nil)
        ((length=1 args) (first args))
        (t (let ((var (generatesym)))
             `(let ((,var ,(first args)))
                (if ,var ,var (or . ,(rest args))))))))

(def-juke-macro cond (&rest clauses)
  (cond ((null clauses) nil)
        ((length=1 (first clauses))
         `(or ,(first clauses) (cond .,(rest clauses))))
        ((starts-with (first clauses) 'else)
         `(begin .,(rest (first clauses))))
        (t `(if ,(first (first clauses))
                (begin .,(rest (first clauses)))
                (cond .,(rest clauses))))))

(def-juke-macro case (key &rest clauses)
  (let ((key-val (generatesym "KEY")))
    `(let ((,key-val ,key))
       (cond ,@(mapcar
                #'(lambda (clause)
                    (if (starts-with clause 'else)
                        clause
                        `((member ,key-val ',(first clause))
                          .,(rest clause))))
                clauses)))))

(def-juke-macro define (name &rest body)
  (if (atom name)
      `(begin (set! ,name . ,body) ',name)
      `(define ,(first name) 
         (lambda ,(rest name) . ,body))))

(def-juke-macro delay (compileutation)
  `(lambda () ,compileutation))

;; let or letrec in scheme, read scheme docs... Letrec, on the other hand, allows you to bind recursive values. 
;; #So you might write a recursive function that you only want to be within the function scope and bind it to a name using letrec.

(def-juke-macro put (bindings &rest body)
  `(let ,(mapcar #'(lambda (v) (list (first v) nil)) bindings)
     ,@(mapcar #'(lambda (v) `(set! .,v)) bindings)
     .,body))
