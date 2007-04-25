
(in-package :weblocks-test)

;;; Test humanize-name function
(deftest humanize-name-1
    (humanize-name 'hello-world)
  "Hello World")

(deftest humanize-name-2
    (humanize-name "HELLO-WORLD")
  "Hello World")

(deftest humanize-name-3
    (humanize-name 'hello-ref)
  "Hello")

;;; Test attributize-name function
(deftest attributize-name-1
    (attributize-name 'hello-world)
  "hello-world")

(deftest attributize-name-2
    (attributize-name "hello World-REF")
  "hello-world-ref")

;;; Test list->assoc function
(deftest list->assoc-1
    (weblocks::list->assoc '(name age (city . location)))
  ((name . name) (age . age) (city . location)))

;;; Introspection helper
(defun class-visible-slot-names (obj &rest args)
  (mapcar #'slot-definition-name
	  (apply #'weblocks::class-visible-slots (class-of obj) args)))

;;; Test class-visible-slots function
(deftest class-visible-slots-1
    (class-visible-slot-names *joe*)
  (name manager))

(deftest class-visible-slots-2
    (class-visible-slot-names *joe* :visible-slots '(age))
  (name age manager))

(deftest class-visible-slots-3
    (class-visible-slot-names *joe* :visible-slots '(age blah))
  (name age manager))

;;; Introspection helper
(defun object-visible-slot-names (obj &rest args)
  (mapcar (lambda (x)
	    (cons (slot-definition-name (car x)) (cdr x)))
	  (apply #'object-visible-slots obj args)))

;;; Test object-visible-slots function
(deftest object-visible-slots-1
    (object-visible-slot-names *joe*)
  ((name . name) (manager . manager)))

(deftest object-visible-slots-2
    (object-visible-slot-names *joe* :slots '((name . "first-name")))
  ((name . "first-name") (manager . manager)))

(deftest object-visible-slots-3
    (object-visible-slot-names *joe* :slots '((name . first-name) age))
  ((name . first-name) (age . age) (manager . manager)))

(deftest object-visible-slots-4
    (object-visible-slot-names *joe* :slots '((name . first-name) (age . how-old)))
  ((name . first-name) (age . how-old) (manager . manager)))

(deftest object-visible-slots-5
    (object-visible-slot-names *joe* :slots '((manager . boss) doesnt-exist))
  ((name . name) (manager . boss)))

(deftest object-visible-slots-6
    (object-visible-slot-names *joe* :slots '(manager) :mode :hide)
  ((name . name)))

(deftest object-visible-slots-7
    (object-visible-slot-names *joe* :slots '(manager name) :mode :strict)
  ((manager . manager) (name . name)))

(deftest object-visible-slots-8
    (object-visible-slot-names *joe* :slots '(manager (name . first-name) (age . how-old))
				     :mode :strict)
  ((manager . manager) (name . first-name) (age . how-old)))

(deftest object-visible-slots-9
    (object-visible-slot-names *joe* :slots '(manager) :mode :strict)
  ((manager . manager)))

;;; test safe-apply
(deftest safe-apply-1
    (safe-apply #'identity '(5))
  5)

(deftest safe-apply-2
    (safe-apply nil '(5))
  nil)

;;; test safe-funcall
(deftest safe-funcall-1
    (safe-funcall #'identity 5)
  5)

(deftest safe-funcall-2
    (safe-funcall nil 5)
  nil)

;;; test request-parameter
(deftest request-parameter-1
    (with-request :get '(("a" . 1) ("b" . 2))
      (request-parameter "a"))
  1)

(deftest request-parameter-2
    (with-request :post '(("a" . 1) ("b" . 2))
      (request-parameter "b"))
  2)

;;; test string-whitespace-p
(deftest string-whitespace-p-1
    (string-whitespace-p "")
  t)

(deftest string-whitespace-p-2
    (string-whitespace-p "   	")
  t)

(deftest string-whitespace-p-3
    (string-whitespace-p " a  	")
  nil)
