(in-package #:weblocks)

(export '(list-presentation))

(defclass list-presentation (text-presentation)
  ()
  (:documentation "A presentation suitable for arbitrary list structure."))

(defmethod render-view-field-value (value (presentation list-presentation)
                                    field view widget obj &rest args
                                    &key &allow-other-keys)
  (with-html
    (:span :class "value"
	   (esc (prin1-to-string value)))))
