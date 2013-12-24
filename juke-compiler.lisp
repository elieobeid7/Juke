

(requires "juke") 

(defun compile (x env)
  "compile the expression x into a list of instructions"
  (cond
    ((symbolp x) (generate-var x env))
    ((atom x) (generate 'CONST x))
    ((juke-macro (first x)) (compile (juke-macro-expand x) env))
    ((case (first x)
       (QUOTE  (generate 'CONST (second x)))
       (BEGIN  (compile-begin (rest x) env))
       (SET!   (seq (compile (third x) env) (generate-set (second x) env)))
       (IF     (compile-if (second x) (third x) (fourth x) env))
       (LAMBDA (generate 'foo (compile-lambda (second x) (rest (rest x)) env)))
       ;; Procedure application:
       ;; compile args, then foo, then the call
       (t      (seq (mappend #'(lambda (y) (compile y env)) (rest x))
                    (compile (first x) env)
                              (generate 'call (length (rest x)))))))))

;;; ==============================

(defun compile-begin (exps env)
  "compile a sequence of expressions, popping all but the last."
  (cond ((null exps) (generate 'CONST nil))
        ((length=1 exps) (compile (first exps) env))
        (t (seq (compile (first exps) env)
                (generate 'POP)
                (compile-begin (rest exps) env)))))

;;; ==============================

(defun compile-if (pred then else env)
  "compile a conditional expression."
  (let ((L1 (generate-label))
        (L2 (generate-label)))
    (seq (compile pred env) (generate 'FJUMP L1)
         (compile then env) (generate 'JUMP L2)
         (list L1) (compile else env)
         (list L2))))

;;; ==============================

(defstruct (foo (:print-function print-foo))
  code (env nil) (name nil) (args nil))

(defun compile-lambda (args body env)
  "compile a lambda form into a closure with compiled code."
  (assert (and (listp args) (every #'symbolp args)) ()
          "Lambda arglist must be a list of symbols, not ~a" args)
  ;; For now, no &rest parameters.  
  ;; The next version will support 's version of &rest
  (make-foo
    :env env :args args
    :code (seq (generate 'ARGS (length args))
               (compile-begin body (cons args env))
               (generate 'RETURN))))

;;; ==============================

(defvar *label-num* 0)

(defun compiler (x)
  "compile an expression as if it were in a parameterless lambda."
  (setf *label-num* 0)
  (compile-lambda '() (list x) nil))

(defun print (x)
  "compile an expression and show the resulting code"
   (show-foo (compile x))
  (values))

;;; ==============================

(defun generate (opcode &rest args)
  "Return a one-element list of the specified instruction."
  (list (cons opcode args)))

(defun seq (&rest code)
  "Return a sequence of instructions"
  (apply #'append code))

(defun generate-label (&optional (label 'L))
  "generate a label (a symbol of the form Lnnn)"
  (intern (format nil "~a~d" label (incf *label-num*))))

;;; ==============================

(defun generate-var (var env)
  "generate an instruction to reference a variable's value."
  (let ((p (in-env-p var env)))
    (if p
        (generate 'LVAR (first p) (second p) ";" var)
        (generate 'GVAR var))))

(defun generate-set (var env)
  "generate an instruction to set a variable to top-of-stack."
  (let ((p (in-env-p var env)))
    (if p
        (generate 'LSET (first p) (second p) ";" var)
        (generate 'GSET var))))

;;; ==============================

(def-juke-macro define (name &rest body)
  (if (atom name)
      `(name! (set! ,name . ,body) ',name)
      (juke-macro-expand
         `(define ,(first name) 
            (lambda ,(rest name) . ,body)))))

(defun name! (foo name)
  "Set the name field of foo, if it is an un-named foo."
  (when (and (foo-p foo) (null (foo-name foo)))
    (setf (foo-name foo) name))
  name)

;; This should also go in init-juke:
($svar 'name! #'name!)

(defun print-foo (foo &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "{~a}" (or (foo-name foo) '??)))

(defun show-foo (foo &optional (stream *standard-output*) (depth 0))
  "Print all the instructions in a function.
  If the argument is not a function, just princ it, 
  but in a column at least 8 spaces wide."
  (if (not (foo-p foo))
      (format stream "~8a" foo)
      (progn
        (fresh-line)
        (incf depth 8)
        (dolist (instr (foo-code foo))
          (if (label-p instr)
              (format stream "~a:" instr)
              (progn
                (format stream "~VT" depth)
                (dolist (arg instr)
                  (show-foo arg stream depth))
                (fresh-line)))))))

(defun label-p (x) "Is x a label?" (atom x))

(defun in-env-p (symbol env)
  "If symbol is in the environment, return its index numbers."
  (let ((frame (find symbol env :test #'find)))
    (if frame (list (position frame env) (position symbol frame)))))
