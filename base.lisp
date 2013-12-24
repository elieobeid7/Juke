
(eval-when (eval compile load)
  ;; Make it ok to place a function definition on a built-in LISP symbol.
  
  ;; this code execute lisp, it looks for lisp compiler 
  #+(or Allegro EXCL)
  (dolist (pkg '(excl common-lisp common-lisp-user))
    (setf (excl:package-definition-lock (find-package pkg)) nil))
  
  

  ;; Don't warn if a function is defined in multiple files -
  ;; this happens often since we refine several programs.
  #+Lispworks
  (setq *PACKAGES-FOR-WARN-ON-REDEFINITION* nil)

  #+LCL 
   (compiler-options :warnings nil)
  )


(defun requires (&rest files)  ;; first is car and rest is cdr
  "The arguments are files that are required to run an application."
  (mapc #'load-juke-file files))

(defvar *juke-files*
  `("base" "juke-compiler" "juke"))

(defparameter *juke-directory*
  (make-pathname :name nil :type nil
     :defaults (or (and (boundp '*load-truename*) *load-truename*)
             (truename "")))
  "The location of the source files.")

(defparameter *juke-source* 
  ;; this is where you can change file extension
  (make-pathname :name nil :type "lisp"  ;; why not make it .juke?!
     :defaults *juke-directory*)) 


;; useless code here
(defparameter *juke-binary*
  (make-pathname
   :name nil
   :type (first (list #+LCL (first *load-binary-pathname-types*)
          #+Lispworks system::*binary-file-type*
          #+MCL "fasl"
          #+Allegro excl:*fasl-default-type*
          #+(or AKCL KCL) "o"
          #+CMU "sparcf"
          #+CLISP "fas"
          "bin")) 
   ;; looking for a compiler
   
   :directory (append (pathname-directory *juke-source*) '("bin"))
   :defaults *juke-directory*))

(defun juke-pathname (name &optional (type :lisp))
  (make-pathname :name name 
     :defaults (ecase type
           ((:lisp :source) *juke-source*)
           ((:binary :bin) *juke-binary*))))
;; the path

(defun compile-all-juke-files ()
  (mapc #'compile-juke-file *juke-files*))

(defun compile-juke-file (name)
  (let ((path (juke-pathname name :lisp)))
    (load path)
    (compile-file path :output-file (juke-pathname name :binary))))

(defun load-juke-file (file)
  "Load the binary file if it exists and is newer, else load the source."
  (let* ((src (juke-pathname file :lisp))
   (src-date (file-write-date src))
   (bin (juke-pathname file :binary))
   (bin-date (file-write-date bin)))
    (load (if (and (probe-file bin) src-date bin-date (>= bin-date src-date))
        bin
      src))))


  (defun rest2 (x)
    "The rest of a list after the first 2 elements."
    (rest (rest x)))


  (defun starts-with (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (eql (first list) x)))



  (defun exp? (exp)
    "Is the expression a constant, variable, or function"
 
    (or (atom exp) (constantp exp)
  (starts-with exp 'function)
  (and (starts-with exp 'the)
       (exp? (third exp)))))



;;;; base Functions

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  
  "Find all those elements of sequence that match item,
  according to the keywords."
  
  (if test-not
      (apply #'remove item sequence 
             :test-not (compiler test-not) keyword-args)
      (apply #'remove item sequence
             :test (compiler test) keyword-args)))

(defun length=1 (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun rest3 (list)
  "The rest of a list after the first THREE elements."
  (cdddr list))

(defun add? (op exps &optional if-nil)
  "add? 'and exps t) returns
  t if exps is nil, exps if there is only one,
  and (and exp1 exp2...) if there are several exps."
  
  
  
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))





(defun symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (make-symbol (format nil "~{~a~}" args)))

(defun last1 (list)
  "Return the last element (not last cons cell) of list"
  (first (last list)))

;;; ==============================

(defun mappend (foo list)
  "Append the results of calling foo on each element of list."
  (apply #'append (mapcar foo list)))

