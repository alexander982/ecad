(vl-load-com)

(setq *doc* (vla-get-activeDocument (vlax-get-acad-object)))
(setq *dicts* (vla-get-dictionaries *doc*))

(setq *mspace* (vla-get-modelSpace *doc*))

(setq *ecd:xrefs* '())

;;(width height left-margin right-margin top-margin bottom-margin)
(setq *ecd:a4-page* (list 210.0 297.0 20.0 5.0 5.0 5.0))
(setq *ecd:a4-zone-number* 6)
(setq *ecd:pages-layout* "table") ;;table or row
(setq *ecd:pages-in-row* 4)


(setq *ecd:dict-name* "ecad")
(setq *ecd:dict* (vla-add *dicts* *ecd:dict-name*))
(setq *ecd:first-page-key* "1st-page")
(setq *ecd:xref-key* "xref")


(defun ecd:get-first-page  (/ pt1 pt2)
  (setq pt1 (getpoint "������� ���� ������� ����� >_"))
  (setq pt2 (getcorner pt1 "������� ��������������� ���� >_"))
  (list (car pt1) (cadr pt1) (car pt2) (cadr pt2)))

(defun ecd:normolize-page-setup  (setup / x1 x2 y1 y2 tmp)
  ;; page setup must be like next picture
  ;;       --+ (x2 y2)
  ;;       | |
  ;;(x1 y1)+--
  (setq x1  (car setup)
        y1  (nth 1 setup)
        x2  (nth 2 setup)
        y2  (nth 3 setup)
        tmp nil)
  (if (> x1 x2)
    (setq tmp x2
          x2  x1
          x1  tmp))
  (if (> y1 y2)
    (setq tmp y2
          y2  y1
          y1  tmp))
  (list x1 y1 x2 y2))

(defun ecd:save-first-page-setup  (setup)
  (vla-add *dicts* *ecd:dict-name*)
  (vlax-ldata-put *ecd:dict-name*
    *ecd:first-page-key*
    (ecd:normolize-page-setup setup)))

(defun ecd:get-item  (coll name / tmp)
  (setq tmp (vl-catch-all-apply
              'vla-item
              (list coll name)))
  (if (vl-catch-all-error-p tmp)
    nil
    tmp))

(defun ecd:get-first-page-setup  ()
  (if (ecd:get-item *dicts* *ecd:dict-name*)
    (vlax-ldata-get *ecd:dict-name* *ecd:first-page-key*)
    nil))

(defun ecd:set-pages-layout (layout)
  (setq *ecd:pages-layout* layout))

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

(defun ecd:point-on-any-page-p (pt / x1 y1 x y page width height)
  ;;check if point on any page according to pages layout
  (setq page (ecd:get-first-page-setup)
        x1 (car page)
        y1 (cadr page)
        x (car pt)
        y (cadr pt)
        width (car *ecd:a4-page*)
        height (cadr *ecd:a4-page*))
  (cond
    ((= "table" *ecd:pages-layout*)
     (and (> x x1)
          (< x (+ x1 (* *ecd:pages-in-row* width)))
          (< y (+ y1 height))))
    ((= "row" *ecd:pages-layout*)
     (and (> x x1)
          (> y y1)
          (< y (+ y1 height))
       ))
    (t nil)
    ))

(defun ecd:column-number (x-start x-current width / column column-number)
  (defun column-number (x x-curr n width)
    (if (and (> x-curr x)
             (< x-curr (+ x width)))
      n
      (column-number (+ x width) x-curr (1+ n) width)))
  (column-number x-start x-current 1 width))

(defun ecd:row-number  (y-start y-current height direction / row-number fn)
  ;;count row number in given direction
  (setq fn (if (eq 'up direction) + -))
  (defun row-number  (y y-curr n height fn)
    (if (and (> y-curr y)
                (< y-curr (+ y height)))
         n
         (row-number (fn y height) y-curr (1+ n) height fn)))
  (row-number y-start y-current 1 height fn))

(defun ecd:page-number  (pt / x y x1 y1 first-page width height)
  (setq x          (car pt)
        y          (cadr pt)
        first-page (ecd:get-first-page-setup)
        x1         (car first-page)
        y1         (cadr first-page)
        width      (car *ecd:a4-page*)
        height     (cadr *ecd:a4-page*))
  (cond
    ((= *ecd:pages-layout* "table")
     (+ (ecd:column-number x1 x width)
        (* *ecd:pages-in-row* (1- (ecd:row-number y1 y height 'down)))))
    ((= *ecd:pages-layout* "row")
     (ecd:column-number x1 x width))))

(defun ecd:zone-number
       (pt / x x1 first-page width height left-margin right-margin page-width)
  (setq x            (car pt)
        first-page   (ecd:get-first-page-setup)
        x1           (car first-page)
        width        (car *ecd:a4-page*)
        left-margin  (nth 2 *ecd:a4-page*)
        right-margin (nth 3 *ecd:a4-page*)
        page-width   (/ (- width left-margin right-margin)
                        *ecd:a4-zone-number*))
  (cond
    ((= *ecd:pages-layout* "table")
     (ecd:column-number
       (+ left-margin (+ x1 (* width (1- (ecd:column-number x1 x width)))))
       x
       page-width))
    ((= *ecd:pages-layout* "row")
     (ecd:column-number
       (+ left-margin (+ x1 (* width (1- (ecd:column-number x1 x width)))))
       x
       page-width))))

(defun ecd:page-zone  (pt)
  (if (ecd:point-on-any-page-p pt)
    (list (ecd:page-number pt) (ecd:zone-number pt))
    (list 0 0)))

(defun ecd:store-simple-links (lst)
  (setq *ecd:xrefs* (append *ecd:xrefs* (list lst))))

(defun ecd:save-xrefs  ()
  ;;permanently save xrefs data in drawing
  (ecd:clean-xrefs)
  (vlax-ldata-put
    *ecd:dict-name*
    *ecd:xref-key*
    (mapcar '(lambda (lst)
               (mapcar 'vla-get-handle lst))
            *ecd:xrefs*)))

(defun ecd:load-xrefs  (/ xrefs)
  ;;load saved xrefs data
  (setq xrefs (vl-remove-if             ;filter out deleted entries
                '(lambda (lst)
                   (vl-some '(lambda (x)
                               (not (handent x)))
                            lst))
                (vlax-ldata-get *ecd:dict-name* *ecd:xref-key*)))
  (setq *ecd:xrefs*
         (mapcar
           '(lambda (lst)
              (mapcar
                '(lambda (h) (vlax-ename->vla-object (handent h)))
                lst))
           xrefs)))

(defun ecd:format-xref-text (page-zone)
  (strcat "(" (itoa (car page-zone)) "." (itoa (cadr page-zone)) ")"))

(defun ecd:link-xrefs  (/ ob1 ob2 xr1 xr2)
  ;; prompt user to select text objects in drawing. Replace 
  ;; text for xref zones. Save pair info
  ;; FIX error if user cancel selection
  (setq ob1 (ecd:pick-object
              "�������� ����� ������ ������������ ������\n")
        ob2 (ecd:pick-object
              "�������� ����� ������ ������������ ������\n")
        xr1 (ecd:page-zone
              (ecd:get-insertion-point ob1))
        xr2 (ecd:page-zone
              (ecd:get-insertion-point ob2)))
  (ecd:store-simple-links (list ob1 ob2))
  (vla-put-textString
    ob1
    (ecd:format-xref-text xr2))
  (vla-put-textString
    ob2
    (ecd:format-xref-text xr1))
  (ecd:save-xrefs)
  (princ))

(defun ecd:update-xrefs  (/ xr1 xr2)
  ;; update zone info in linked xrefs
  (ecd:clean-xrefs)
  (foreach lst  *ecd:xrefs*
    (progn
      (setq xr1 (ecd:page-zone
                  (ecd:get-insertion-point (car lst)))
            xr2 (ecd:page-zone
                  (ecd:get-insertion-point (cadr lst))))
      (vla-put-textString
        (car lst)
        (ecd:format-xref-text xr2))
      (vla-put-textString
        (cadr lst)
        (ecd:format-xref-text xr1))))
  (princ))

(defun ecd:delete-xrefs ()
  ;; delete all xrefs info from drawing
  (setq *ecd:xrefs* nil)
  (vlax-ldata-delete *ecd:dict-name* *ecd:xref-key*)
  (princ))

(defun ecd:clean-xrefs  ()
  ;; clean from deleted objects
  (setq *ecd:xrefs*
         (vl-remove-if
           '(lambda (lst) (vl-some 'vlax-erased-p lst))
           *ecd:xrefs*)))

(ecd:load-xrefs)

;|�Visual LISP� Format Options�
(80 2 40 0 nil "end of " 80 9 0 0 1 nil T T T)
;*** DO NOT add text below the comment! ***|;