
(in-package :weblocks)

(export '(radio radio-presentation))

;;; Radio buttons
(defclass radio-presentation (form-presentation choices-presentation-mixin)
  ())

(defmethod render-form-presentation ((presentation radio-presentation)
				     label-fn value-fn wrap-fn)
  (declare (ignore value-fn))
  (funcall wrap-fn (curry label-fn t)))

(defmethod render-view-field-value (value (presentation radio-presentation)
				    (field form-view-field) (view form-view) widget obj
				    &rest args
				    &key intermediate-values field-info &allow-other-keys)
  (declare (ignore args)
	   (special *presentation-dom-id*))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (render-radio-buttons (if field-info
                            (attributize-view-field-name field-info)
                            (attributize-name (view-field-slot-name field)))
			  (obtain-presentation-choices presentation obj)
			  :selected-value (if intermediate-value-p
					      intermediate-value
					      (when value
						(attributize-name value)))
			  :disabledp (form-view-field-disabled-p field obj)
			  :id *presentation-dom-id*)))

