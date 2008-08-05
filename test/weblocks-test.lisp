
(defpackage #:weblocks-test
  (:use :cl :weblocks :rtest :c2mop :cl-who :hunchentoot :metatilities :moptilities)
  (:shadowing-import-from :c2mop #:defclass #:defgeneric #:defmethod
			  #:standard-generic-function #:ensure-generic-function #:standard-class
			  #:typep #:subtypep)
  (:shadowing-import-from :weblocks #:redirect)
  (:shadow #:do-test #:do-tests #:continue-testing)
  (:export #:test-weblocks #:do-pending))

(in-package :weblocks-test)

(defmacro with-test-environment (&body body)
  "This macro takes steps to clear the environment for the unit
tests. For example, if testing in the context of an application, it
may interfere with the unit tests, so we remove the application. All
changes are rolled back after the tests are done. Note, the steps that
this macro takes may not be sufficient. If some tests fail, try to run
the test suite without loading an application."
  `(let ((weblocks::*current-webapp* nil) ;hide application
	 (*print-case* :upcase)		;needed by some comparison output
	 interference-methods result)
     (declare (special weblocks::*current-webapp*))
     ;; remove before/after methods from render-page-body
     (mapcar (lambda (m)
	       (let ((qualifiers (method-qualifiers m)))
		 (when (or (find :before qualifiers)
			   (find :after qualifiers))
		   (remove-method #'render-page-body m)
		   (push m interference-methods))))
	     (generic-function-methods #'render-page-body))
     ;; insert the body
     (setf result (progn ,@body))
     ;; reinstate render-page-body before/after methods
     (loop for m in interference-methods
	do (add-method #'render-page-body m))
     result))

(defun do-test ()
  "Shadows rt's 'do-test'. This function calls rt's do test in a clean
test environment. See 'with-test-environment'."
  (with-test-environment
      (rtest::do-test)))

(defun do-tests ()
  "Shadows rt's 'do-tests'. This function calls rt's do-tests in a
clean test environment. See 'with-test-environment'."
  (with-test-environment
      (rtest::do-tests)))

(defun test-weblocks ()
  "Call this function to run all unit tests defined in 'weblocks-test'
package. This function tests weblocks in a clean environment. See
'with-test-environment' for more details."
  (do-tests))

(defun continue-testing ()
  "Shadows rt's 'continue-testing'. This function calls rt's
continue-testing in a clean test environment. See
'with-test-environment'."
  (with-test-environment
      (rtest::continue-testing)))

(defun do-pending ()
  "An alias for 'continue-testing'."
  (continue-testing))

(defparameter *test-widget-id* 0
  "Used to generate a unique ID for fixtures.")

; We'll use memory store for testing
(defstore *not-searchable-store* :memory)
(defstore *test-store* :memory)

(defmacro deftest-html (name form value)
  "A helper macro for creating html test cases. The macro writes
code that temporarily binds the output stream to a string stream
and then compares the string to the expected result."
  (let ((expected-result (eval
			  `(with-html-output-to-string (s)
			     ,value))))
    `(deftest ,name
	 (let ((stream-bak *weblocks-output-stream*)
	       (*test-widget-id* 1000)
	       result)
	   (declare (special *test-widget-id*))
	   (setf *weblocks-output-stream* (make-string-output-stream))
	   ,form
	   (setf result (get-output-stream-string *weblocks-output-stream*))
	   (setf *weblocks-output-stream* stream-bak)
	   result)
       ,expected-result)))

;;; faking hunchentoot's requests
(defclass unittest-request ()
  ((headers-in :initform nil)
   method server-protocol
   (hunchentoot::uri :initform nil)
   (content-stream :reader content-stream)
   (cookies-in :initform nil)
   (get-parameters :initform nil)
   (post-parameters :initform nil)
   (script-name :initform nil)
   (query-string :initform nil)
   (session :initform nil
            :accessor hunchentoot::session)
   (aux-data :initform nil
             :accessor hunchentoot::aux-data)
   (raw-post-data :initform nil))
  (:documentation "A class used to mock hunchentoot requests in
  order to be able to unit test across requests."))

;;; faking hunchentoot's server
(defclass unittest-server ()
  ((mod-lisp-p :initform nil
               :initarg :mod-lisp-p
               :reader hunchentoot::server-mod-lisp-p)
   (ssl-certificate-file :initarg :ssl-certificate-file
			 :initform nil
                         :reader hunchentoot::server-ssl-certificate-file))
  (:documentation "A class used to mock hunchentoot server in
  order to be able to unit test across requests."))

(defmethod hunchentoot::server-mod-lisp-p ((obj unittest-server))
  (slot-value obj 'mod-lisp-p))

(defparameter *dummy-action* "abc"
  "A dummy action code for unit tests.")

(defun call-with-webapp (thunk &rest initargs &key full class-name &allow-other-keys)
  "Helper for `with-webapp''s expansion."
  (remf initargs :full)
  (remf initargs :class-name)
  (let* ((app (apply #'make-instance (or class-name 'weblocks::weblocks-webapp)
		     initargs))
	 (weblocks::*current-webapp* app))
     (declare (special weblocks::*current-webapp*))
     (cond (full
	    (assert class-name (class-name)
		    "A specific webapp must be defined for `with-webapp's :full parameter")
	    (start-webapp class-name)
	    (unwind-protect (funcall thunk)
	      (stop-webapp class-name)))
	   (t (funcall thunk)))))

(defmacro with-webapp ((&rest initargs &key full class-name &allow-other-keys) &body body)
  "A helper macro (and marker) for test cases calling functions that
only work with a current webapp, in which BODY is evaluated in the
context of a temporary `weblocks-webapp'.  INITARGS are passed through
to `make-instance'.

If FULL is given, I will also start the webapp within BODY's context;
if CLASS-NAME is given, I will ignore other INITARGS and either find
or start a webapp with the class CLASS-NAME, setting it as the current
webapp in my context."
  (declare (ignore full class-name))
  `(call-with-webapp (lambda () ,@body) ,@initargs))

(defun call-with-request-in-webapp-context (thunk method parameters
					    &key (uri nil uri?))
  "Helper for `call-with-request'."
  (let ((parameters-slot (ecase method
			   (:get 'get-parameters)
			   (:post 'post-parameters))))
    (let* ((*request* (make-instance 'unittest-request))
	   (*server* (make-instance 'unittest-server))
	   (hunchentoot::*remote-host* "localhost")
	   (hunchentoot::*session-secret* (hunchentoot::reset-session-secret))
	   (hunchentoot::*reply* (make-instance 'hunchentoot::reply))
	   (make-action-orig #'weblocks::make-action)
	   (generate-widget-id-orig #'weblocks::gen-id)
	   (dummy-action-count 123)
	   (*session-cookie-name* "weblocks-session")
	   (*uri-tokens* '("foo" "bar"))
	   weblocks::*page-dependencies* *session*
	   *on-ajax-complete-scripts*)
      (declare (special *uri-tokens* weblocks::*page-dependencies* *session*
			*on-ajax-complete-scripts*))
      (unwind-protect (progn
			(weblocks::open-stores)
			(start-session)
			(setf (symbol-function 'weblocks::make-action)
			      (lambda (action-fn &optional action-code)
				(if action-code
				    (funcall make-action-orig action-fn action-code)
				    (let ((result (funcall make-action-orig action-fn
							   (format nil "~A~D"
								   *dummy-action*
								   dummy-action-count))))
				      (incf dummy-action-count)
				      result))))
			(setf (symbol-function 'weblocks::gen-id)
			      (lambda ()
				"id-123"))
			(setf (slot-value *request* 'method) method)
			(setf (slot-value *request* parameters-slot) parameters)
			(setf (slot-value *request* 'hunchentoot::script-name) "/foo/bar")
			(setf (slot-value *session* 'hunchentoot::session-id) 1)
			(setf (slot-value *session* 'hunchentoot::session-string) "test")
			(when uri?
			  (setf (slot-value *request* 'hunchentoot::uri) uri))
			(funcall thunk))
	(setf (symbol-function 'weblocks::make-action) make-action-orig)
	(setf (symbol-function 'weblocks::gen-id) generate-widget-id-orig)
	(weblocks::close-stores)))))

(defun call-with-request (&rest args)
  "Helper for `with-request''s expansion."
  (if (and (boundp 'weblocks::*current-webapp*)
	   (let ((app-type (class-name (class-of (weblocks::current-webapp)))))
	     (or (eq 'weblocks-webapp app-type)
		 (equalp "WEBLOCKS-TEST"
			 (package-name (symbol-package app-type))))))
      (apply #'call-with-request-in-webapp-context args)
      (with-webapp ()
	(apply #'call-with-request-in-webapp-context args))))

(defun span-keyword-params (implicit-progn)
  "Take the plist from the front of IMPLICIT-PROGN, and answer it and
the remaining forms.  Note that there must be at least one non-plist
form, because we want to always maintain \"returns value of last
form\" semantics to reduce any confusion caused by this syntax.

Anyway, if you want to force interpretation of a keyword in
IMPLICIT-PROGN as a prognable form, just quote it."
  (loop for forms on implicit-progn by #'cddr
     while (typep forms '(cons keyword (cons t cons)))
     append (subseq forms 0 2) into plist
     finally (return (values plist forms))))

(defmacro with-request (method parameters &body body)
  "A helper macro for test cases across web requests. The macro
sets up a request and a session, and executes code within their
context.

'method' - A method with which the request was initiated (:get
or :post)
'parameters' - An association list of parameters sent along with
the request.

Other parameters given as keywords on the front of BODY:

URI - Set the Hunchentoot request URI to this."
  (multiple-value-bind (kwargs body) (span-keyword-params body)
    `(call-with-request (lambda () ,@body) ,method ,parameters ,@kwargs)))

(defun do-request (parameters)
  "Mocks up a submitted request for unit tests."
  (setf (slot-value *request* (ecase (request-method)
				(:get 'get-parameters)
				(:post 'post-parameters))) parameters)
  (weblocks::eval-action))

(defun make-request-ajax ()
  "Adds appropriate headers to a request so it is considered to be an
AJAX request."
  (push '("X-Requested-With" . "test") (slot-value *request* 'headers-in)))
