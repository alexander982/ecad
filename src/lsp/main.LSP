(vl-load-com)

(defun ecd:make-2d-point  (x y)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray vlax-vbDouble '(0 . 1))
      (list x y))))

(defun ecd:move-view-to-point (x y / view)
  (setq view (vla-get-ActiveViewport *doc*))
  (vla-put-center view (ecd:make-2d-point x y))
  (vla-put-height view *ecd:viewport-height*)
  (vla-put-ActiveViewport *doc* view))

(defun ecd:pick-object  (msg)
  ;;prompt user to select object
  ;; @msg - message for prompt
  ;; @return - selected vla-object
  ;; FIX error when user cancel selection
  (vlax-ename->vla-object
             (car (entsel msg))))

(defun ecd:get-insertion-point  (obj / pt)
  (setq pt (vla-get-insertionPoint obj))
  (vlax-safearray->list (vlax-variant-value pt)))

(defun c:insertxr ()
  (if *ecd:settings-present*
    (ecd:insert-xrefs)
    (ecd:setup-page-dialog)))

(defun c:linkxr  ()
  (if *ecd:settings-present*
    (ecd:link-xrefs)
    (ecd:setup-page-dialog)))

(defun c:followxr ()
  (if *ecd:xrefs*
    (ecd:follow-xref)
    (princ)))

(defun c:updatexr  ()
  (if *ecd:xrefs*
    (ecd:update-xrefs)
    (princ)))

(defun c:xsettings ()
  (ecd:setup-page-dialog))

(princ "ecad loading...\n")

(ecd:load-page-setup)
(ecd:load-xrefs)

(princ (strcat "Íàéäåíî " (itoa (length *ecd:xrefs*)) " ïàğ ññûëîê\n"))

;|«Visual LISP© Format Options»
(80 2 40 0 nil "end of " 80 9 0 0 1 nil T T T)
;*** DO NOT add text below the comment! ***|;
