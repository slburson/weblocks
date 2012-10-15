
(in-package :weblocks)

(export '(datagrid-drilldown-field))

;;; Utilities
(defun datagrid-drilldown-style (slot-name)
  "Returns a style used in datagrid drilldown cells."
  (concatenate 'string "drilldown" " " (attributize-name slot-name)))

;;; Custom drilldown field
(defclass datagrid-drilldown-field (table-view-field)
  ((allow-sorting-p :initform nil))
  (:documentation "A field used to render drilldown control."))

(defun make-drilldown-field (grid-obj)
  "Makes a custom field for rendering drilldown controls."
  (let ((label (humanize-name (car (dataseq-on-drilldown grid-obj)))))
    (make-instance 'datagrid-drilldown-field
		   :label label
		   :reader label
		   :present-as nil)))

;;; Drilldown cells
(defmethod render-view-field-header ((field datagrid-drilldown-field) (view table-view)
				     (widget datagrid) presentation value obj &rest args)
  (declare (ignore args))
  (with-html (:th :class (datagrid-drilldown-style
			  (car (dataseq-on-drilldown widget)))
		  "")))

(defmethod render-view-field ((field datagrid-drilldown-field) (view table-view)
			      (widget datagrid) presentation value obj &rest args
			      &key row-action &allow-other-keys)
  (declare (ignore args))
  (unless (ajax-request-p)
    (with-html
      (:td :class (datagrid-drilldown-style (car (dataseq-on-drilldown widget)))
	   (:noscript
	     (:span :class "non-ajax-drilldown-link"
		    (let ((label (view-field-label field)))
		      (if (stringp row-action)
			  (htm (:a :href row-action
			       (str label)))
			(render-link row-action label :ajaxp nil)))))))))

(defmethod with-table-view-header ((view table-view) obj (widget datagrid)
				   header-fn rows-fn
				   &rest args &key summary &allow-other-keys)
  (if (and (dataseq-allow-drilldown-p widget)
	   (or (dataseq-on-drilldown widget) (dataseq-drilldown-link-url-fn widget)))
      ;; &&& Hmm -- hate to copy this here just to add a CSS class
      (with-html
      ;; &&& Need option for condensed
	(:table :class "table table-condensed table-hover"
		:summary (or summary (table-view-default-summary view))
		;; See dataseq-render-mining-bar
		;(when (view-caption view)
		;  (htm (:caption (str (view-caption view)))))
		(htm
		 (:thead
		  (apply header-fn view (car obj) widget args))
		 (:tbody
		  (apply rows-fn view obj widget args)))))
    (call-next-method)))

;;; Drilldown row
(defmethod with-table-view-body-row ((view table-view) obj (widget datagrid) &rest args
				     &key alternp &allow-other-keys)
  (if (and (dataseq-allow-drilldown-p widget)
	   (or (dataseq-on-drilldown widget) (dataseq-drilldown-link-url-fn widget)))
      (let ((row-action (if (dataseq-drilldown-link-url-fn widget)
			    (funcall (dataseq-drilldown-link-url-fn widget) widget obj)
			  (make-action
			    (lambda (&rest args)
			      (declare (ignore args))
			      (when (dataseq-autoset-drilled-down-item-p widget)
				(setf (dataseq-drilled-down-item widget) obj))
			      (funcall (cdr (dataseq-on-drilldown widget)) widget obj)))))
	    (drilled-down-p (and (dataseq-drilled-down-item widget)
				 (eql (object-id (dataseq-drilled-down-item widget))
				      (object-id obj)))))
	(safe-apply (sequence-view-row-prefix-fn view) view obj args)
	(with-html
	  (:tr :class (append-css-classes (and alternp "table-striped-row")
					  (and drilled-down-p "drilled-down")
					  (funcall (table-view-row-class-fn view) obj))
	       :onclick (if (dataseq-drilldown-link-url-fn widget)
			    (format nil "window.location.assign(\"~A\");"
				    (funcall (dataseq-drilldown-link-url-fn widget) widget obj))
			  (format nil "initiateActionOnEmptySelection(\"~A\", \"~A\");"
				  row-action (session-name-string-pair)))
	       :onmouseover "this.style.cursor = \"pointer\";"
	       :style "cursor: expression(\"hand\");"
	       (apply #'render-table-view-body-row view obj widget :row-action row-action args)))
	(safe-apply (sequence-view-row-suffix-fn view) view obj args))
      (call-next-method)))

(defmethod render-view-field-value (value (presentation text-presentation) (field table-view-field)
				    view (widget datagrid) obj
				    &rest args)
  (declare (ignore args))
  (if (eq (view-field-slot-name field) (dataseq-drilldown-link-field widget))
      (with-html
	(:a :href (funcall (dataseq-drilldown-link-url-fn widget) widget obj)
	    (call-next-method)))
    (call-next-method)))
