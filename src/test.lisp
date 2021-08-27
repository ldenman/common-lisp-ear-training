(in-package :ld-music)

(defvar *test-name* nil)
(defvar *tests* nil)
(defun reset-tests ()
  (setf *tests* nil))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  (register-test name)
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun register-test (name)
  (if (not (member name *tests*))
      (push name *tests*)))

(defun run-tests ()
  (let ((result t))
    (loop for test in *tests* collect
      (unless (funcall test) (setf result nil)))
    result))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (if result
      result
      (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
)



