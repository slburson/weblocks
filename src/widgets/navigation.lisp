
(in-package :weblocks)

(export '(navigation render-navigation-menu init-navigation make-navigation
	  navigation-pane-names navigation-menu-items navigation-header
          navigation-hidden-panes navigation-render-content-p))

(export '(lazy-navigation make-lazy-navigation))

(defwidget navigation (static-selector)
  ((pane-names :accessor navigation-pane-names
	       :initarg :pane-names
	       :initform nil
	       :documentation "An alist mapping uri-tokens to
	       human-readable pane names (rendered as a menu). Use nil
	       as the key for the default item.")
   (header :accessor navigation-header
	   :initarg :header
	   :initform nil
	   :documentation "A heading for the menu.")
   (hidden-panes :accessor navigation-hidden-panes
		 :initarg :hidden-panes
		 :initform nil
		 :documentation "A list of uri-tokens representing a set
		 of panes that should be hidden (not rendered in a menu,
		 but accessible from within this navigation object.)  Or,
		 a function of no arguments returning such a list; it will
		 be called each time the navigation is rendered.")
   (render-content-p :accessor navigation-render-content-p
		   :initarg :render-content-p
		   :initform t
		   :documentation "Whether navigation should also render
		   its contents. You want to set this to nil if you use
		   the teleport widget to render the contents
		   elsewhere.")
   (extra-menu-items :accessor navigation-extra-menu-items
		     :initarg :extra-menu-items
		     :initform nil
		     :documentation "Additional items for RENDER-MENU.")
   (disabled-panes :accessor navigation-disabled-panes
		   :initarg :disabled-panes
		   :initform nil
		   :documentation "Panes in this list are rendered as
		   visible but disabled. This feature can be useful for
		   section labels within a menu, for example.")
   (menu-style :accessor navigation-menu-style
	       :initarg :menu-style
	       :initform '(:sidebar :left)
	       ;; Many styles are possible with Bootstrap.  Currently implemented:
	       ;;  (:sidebar :left)
	       ;;  (:tabs :top)
	       ;; Planned but not yet implemented:
	       ;;  (:navbar :top)  [defaults to static]
	       ;;  (:navbar :top :fixed)
	       :documentation "A list of a style type, a position, and zero or more
	       options.  The style type is one of ':sidebar, ':tabs, or ':navbar.
	       The position is one of ':left, ':top, ':right, or ':bottom.  The
	       navbar style takes the ':fixed option."))
  (:documentation "The navigation widget can act as a menu controls, a
  tabbed control, etc. It is a static-selector that also knows what its
  pane names are, so it can render a menu, set a page title, and
  contribute to navigation breadcrumbs."))

(defmethod initialize-instance :after ((nav navigation) &rest args)
  (declare (ignore args))
  (check-navigation-menu-style nav))

(defgeneric check-navigation-menu-style (nav)
  (:method ((nav navigation))
    (let ((style (navigation-menu-style nav)))
      (unless (member (first style) '(:sidebar :tabs :navbar))
	(error "The first element of the menu-style of a navigation must be one of :sidebar, :tabs, or :navbar; not ~S"
	       (car style)))
	     (unless (member (second style) '(:left :top :right :bottom))
	       (error "The second element of the menu-style of a navigation must be one of :left, :right, :top, or :bottom; not ~S"
		      (second style))))))

(defwidget lazy-navigation (navigation)
  ()
  (:documentation "Lazy navigation does not create the entire widget
  tree immediately. Instead, parts of the widget tree are created as
  they are needed."))

(defun navigation-pane-name-for-token (navigation token)
  "Return the pane name for a given uri-token or NIL if not found. Token
may be NIL in which case the default pane name is provided."
  (cdr (assoc token (navigation-pane-names navigation) :test #'equalp)))

(defgeneric navigation-menu-items (obj)
  (:documentation "Returns the menu items for a navigation object
  in a format suitable for RENDER-MENU. Hidden panes will not be included.")
  (:method ((obj navigation))
    (let* ((nhp (navigation-hidden-panes obj))
	   (hidden-panes (if (functionp nhp) (funcall nhp)
			   nhp)))
      (append
	(remove nil
		(mapcar (lambda (pane)
			  (let ((token (car pane)))
			    (unless (member token hidden-panes
					    :test #'string-equal)
			      (cons (navigation-pane-name-for-token obj token)
				    (uri-tokens-to-string token)))))
			(static-selector-panes obj)))
	(navigation-extra-menu-items obj)))))

(defgeneric render-navigation-menu (obj &rest args)
  (:documentation "Renders the HTML menu for the navigation widget.")
  (:method ((obj navigation) &rest args &key menu-args &allow-other-keys)
    (declare (ignore args))
    (apply #'render-menu (navigation-menu-items obj)
           :base (selector-base-uri obj)
           :selected-pane (or (static-selector-current-pane obj)
			      (caar (static-selector-panes obj)))
           :header (navigation-header obj)
           :container-id (dom-id obj)
           :empty-message "No navigation entries"
           :disabled-pane-names (navigation-disabled-panes obj)
	   menu-args)))

(defgeneric render-navigation-body (nav &rest args)
  (:method ((nav navigation) &rest args)
    (when (navigation-render-content-p nav)
      (with-html 
	(mapc (lambda (kid) (apply #'render-widget kid args))
	      (widget-children nav :selector))))))

(defgeneric render-navigation-styled (nav style-kind style-pos &rest args)
  (:method ((nav navigation) (style-kind (eql ':sidebar)) (style-pos (eql ':left)) &rest args)
    (with-html
      (:div :class "container-fluid"
	    (:div :class "row-fluid"
		  (:div :class "nav-sidebar-menu"
			(apply #'render-navigation-menu nav :menu-args
			       '(:list-class "nav nav-list") args))
		  (:div :class "nav-sidebar-body"
			(apply #'render-navigation-body nav args))))))
  (:method ((nav navigation) (style-kind (eql ':sidebar)) (style-pos (eql ':right)) &rest args)
    (with-html
      (:div :class "container-fluid"
	    (:div :class "row-fluid"
		  (:div :class "nav-sidebar-body"
			(apply #'render-navigation-body nav args))
		  (:div :class "nav-sidebar-menu"
			(apply #'render-navigation-menu nav :menu-args
			       '(:list-class "nav nav-list") args))))))
  (:method ((nav navigation) (style-kind (eql ':tabs)) style-pos &rest args)
    (ecase style-pos
      ((:left :right :top)
	 (with-html
	   (:div :class (append-css-classes "tabbable" (case style-pos
							 (:left "tabs-left")
							 (:right "tabs-right")))
		 (apply #'render-navigation-menu nav :menu-args '(:list-class "nav nav-tabs") args)
		 (:div :class "tab-content"
		       (:div :class "tab-pane active"
			     (apply #'render-navigation-body nav args))))))
      ((:bottom)
	 (with-html
	   (:div :class "tabbable tabs-bottom"
		 (:div :class "tab-content"
		       (:div :class "tab-pane active"
			     (apply #'render-navigation-body nav args)))
		 (apply #'render-navigation-menu nav :menu-args '(:list-class "nav nav-tabs") args)))))))

(defmethod render-widget-body ((obj navigation) &rest args)
  (let ((style (navigation-menu-style obj)))
    (with-html
      (apply #'render-navigation-styled obj (first style) (second style) args))))

(defmethod render-widget-children ((obj navigation) &rest args)
  (declare (ignore args)))

(defmethod page-title ((obj navigation))
  (navigation-pane-name-for-token obj (static-selector-current-pane obj)))

(defun init-navigation (obj &rest args)
  "A helper function to create a navigation widget.
  
  The elements of ARGS are either lists of the form
  (NAME WIDGET [URI-TOKEN]) or flat pairs of atoms,
  in which case the first atom will be the name
  and the second atom the widget.
  
  The two forms of elements may be mixed, but an atom
  must always be followed by another atom.
  
  MAKE-WIDGET is applied to the widget argument so
  you can use strings and function designators as widgets
  in this context."
  ;; normalize args
  (setf args (loop for arg = (pop args)
                   while arg
                   if (atom arg)
                     collect (list arg (pop args))
                   else
                     collect arg))
  ;; initialize nav from args
  (mapc
    (lambda (pane-info)
      (let ((token (ecase (length pane-info)
                     (2 (attributize-name (first pane-info)))
                     (3 (third pane-info))))
            (name (first pane-info))
            (widget (if (and (typep obj 'lazy-navigation)
                             (typep (second pane-info) 'function))
                      (second pane-info)
                      (make-widget (second pane-info)))))
        (when (string-equal token "")
          (setf token nil))
        (push-end (cons token name) (navigation-pane-names obj))
        (push-end (cons token widget) (static-selector-panes obj))))
    args)
  obj)

(defun make-navigation (name &rest args)
  "Instantiates the default navigation widget via 'make-instance' and
forwards it along with 'args' to 'init-navigation'. The navigation
widgets bears the title NAME."
  (let ((nav (apply #'make-instance (or (safe-getf args :navigation-class) 'navigation)
                                    :name name
                                    (safe-getf args :extra-args))))
    (setf args (remove-keyword-parameters args :navigation-class :extra-args))
    (apply #'init-navigation nav args)
    nav))


(defmethod static-selector-get-pane ((navigation lazy-navigation) token)
  "Lazily resolve the pane. Also ensures that the resulting
widget will get cached instead of the generator."
  (let ((pane (call-next-method)))
    (cons
      (car pane)
      (if (functionp (cdr pane)) (make-widget (funcall (cdr pane))) (cdr pane)))))

(defun make-lazy-navigation (name &rest args)
  "Convenience function to create a lazy navigation."
  (apply #'make-navigation name (append args '(:navigation-class lazy-navigation))))


(export '(teleport teleport-source teleport-key))

(defwidget teleport ()
  ((source :accessor teleport-source
	   :initarg :source
	   :documentation "Source widget that should be teleported and
	   rendered.")
   (key :accessor teleport-key
	:initarg :key
	:initform #'identity
	:documentation "The function that will be used to access the
   widget from the source."))
  (:documentation "A widget that will render ('teleport') another widget
  to a particular place. It is your responsibility to make sure the
  teleported widget isn't rendered in its source location. A good use
  for this is navigation with detached content -- you often want to have
  the menu for your navigation rendered in one place, and its navigated
  content elsewhere. The teleport widget lets you do that."))

(defmethod render-widget-body ((obj teleport) &rest args)
  (apply #'render-widget (funcall (teleport-key obj) (teleport-source obj)) args))

