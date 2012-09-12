
(in-package :weblocks)

(export '(*menu-empty-message* render-menu))

(defparameter *menu-empty-message* "No menu entries."
  "A default message shown by 'render-menu' if no entries are
  available.")

(defun render-menu (options &key selected-pane header (container-id (gen-id)) (base "")
                    div-class list-class ordered-list-p (empty-message *menu-empty-message*)
                    disabled-pane-names)
  "Renders a menu snippet based on given options and selected
option. An option may be a dotted pair of a label and \(URL to link to
or function to call on the item's label and selection state \(a boolean)),
or a name (which will be converted to a label and a URL via
humanize-name and attributize-name, respectively). The selected-pane
will be compared to an option's URL via equalp. If the selected
option isn't specified, the first option is rendered as selected.  If
CONTAINER-ID is provided, it is used as the basis of DOM IDs for the
menu and each menu item generated with `unattributized-name'. If a
given pane name is found in `disabled-pane-names', it's rendered in
the navigation as disabled."
  (flet ((render-menu-items ()
           (loop
              for option in options
              for item-number from 1
              do (progn
                   (unless (consp option)
                     (setf option
                           (cons (humanize-name option)
                                 (attributize-name option))))
                   (let* ((label (car option))
                          (target (cdr option))
			  (pane-selected-p (equalp target (or selected-pane "")))
                          (pane-disabled-p (unless (functionp label)
                                             (member (attributize-name label)
                                                     disabled-pane-names
                                                     :key #'attributize-name
                                                     :test #'string-equal)))
                          (pane-class (cond
                                        (pane-selected-p "active")
                                        (pane-disabled-p "disabled"))))
                     (if (functionp label)
			 (funcall label)
                       (with-html
                         (:li :id (unattributized-name (format nil "~A-~A" container-id label)
                                                       'menu-item)
                              :class (append-css-classes pane-class
							 (and ordered-list-p
							      (format nil "item-number-~A"
								      item-number)))
                              (etypecase target
				(string
				 (if pane-disabled-p
				     (htm "&nbsp;")
				   (htm (:a :href
					    (concatenate 'string
							 (string-right-trim "/" base)
							 "/"
							 (string-left-trim "/" target))
					    (str label)))))
				(function
				 (funcall target label pane-selected-p)))))))))))
    (with-html
      (when header
	(htm (:h4 (str header))))
      (if (null options)
	  (htm (:div :class "empty-menu" (str empty-message)))
	(if ordered-list-p
	    (htm (:ol :class list-class (render-menu-items)))
	  (htm (:ul :class list-class (render-menu-items))))))))

