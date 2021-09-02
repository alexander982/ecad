(vl-load-com)

(setq *ecd:xrefs* '())
(setq *ecd:color-interface-object* nil)


(defun ecd:point-on-any-page-p (pt / x1 y1 x y page width height)
  ;;check if point on any page according to pages layout
  (setq page (ecd:get-first-page-setup)
        x1 (car page)
        y1 (cadr page)
        x (car pt)
        y (cadr pt)
        width (car *ecd:page*)
        height (cadr *ecd:page*))
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
        width      (car *ecd:page*)
        height     (cadr *ecd:page*))
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
        width        (car *ecd:page*)
        left-margin  (nth 2 *ecd:page*)
        right-margin (nth 3 *ecd:page*)
        page-width   (/ (- width left-margin right-margin)
                        *ecd:horizontal-zones*))
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

(defun ecd:in-xrefs-p  (obj)
  ;; check if object is in xrefs list
  (vl-some
    '(lambda (l)
      (vl-some
        '(lambda (o) (equal o obj))
        l))
    *ecd:xrefs*))

(defun ecd:xref-target  (obj / pair)
  ;;return xref target
  (setq pair (car (vl-member-if
                    '(lambda (lst)
                       (vl-some
                         '(lambda (o) (equal o obj))
                         lst))
                    *ecd:xrefs*)))
  (if (equal (car pair) obj)
    (cadr pair)
    (car pair)))

(defun ecd:format-xref-text (page-zone)
  (strcat "(" (itoa (car page-zone)) "." (itoa (cadr page-zone)) ")"))

(defun ecd:insert-xrefs  (/ pt1 pt2 ob1 ob2 xr1 xr2 tc)
  ;;TODO add boundign box draw for pick point prompt
  (if (setq pt1 (getpoint "\nУкажите точку вставки первой ссылки: "))
    (progn
      (setq tc (ecd:i-acad-color))
      (vla-put-ColorIndex tc *ecd:xref-color*)
      (setq ob1 (vla-addtext *mspace* "(0.0)" (vlax-3d-point pt1) *ecd:xref-height*))
      (vla-put-TrueColor ob1 tc)
      (if (setq pt2 (getpoint "\nУкажите точку втавки второй ссылки:"))
        (progn
          (setq ob2 (vla-addtext
                      *mspace*
                      (ecd:format-xref-text (ecd:page-zone pt1))
                      (vlax-3d-point pt2)
                      *ecd:xref-height*))
          (vla-put-TrueColor ob2 tc)
          (vla-put-textString
            ob1
            (ecd:format-xref-text (ecd:page-zone pt2)))
          (ecd:store-simple-links (list ob1 ob2))
          (ecd:save-xrefs))
        (vla-delete ob1))))
  (princ))

(defun ecd:link-xrefs  (/ ob1 ob2 xr1 xr2 tc)
  ;; prompt user to select text objects in drawing. Replace 
  ;; text for xref zones. Save pair info
  (if (setq ob1 (ecd:pick-object
                  "Выберите текст первой перекрестной ссылки\n"))
    (progn
      (setq tc (ecd:i-acad-color))
      (vla-put-ColorIndex tc *ecd:xref-color*)
      (vla-highlight ob1 :vlax-true)
      (if (setq ob2 (ecd:pick-object
                      "Выберите текст второй перекрестной ссылки\n"))
        (progn
          (vla-highlight ob2 :vlax-true)
          (setq xr1
                    (ecd:page-zone
                      (ecd:get-insertion-point ob1))
                xr2
                    (ecd:page-zone
                      (ecd:get-insertion-point ob2)))
          (ecd:store-simple-links (list ob1 ob2))
          (vla-put-textString
            ob1
            (ecd:format-xref-text xr2))
          (vla-put-textString
            ob2
            (ecd:format-xref-text xr1))
          (vla-put-TrueColor ob1 tc)
          (vla-put-TrueColor ob2 tc)
          (ecd:save-xrefs)
          (vla-highlight ob2 :vlax-false)))
      (vla-highlight ob1 :vlax-false)))
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

(defun ecd:xrefs-set-color (color)
  (setq tc (ecd:i-acad-color))
  (vla-put-ColorIndex tc color)
  (foreach pair *ecd:xrefs*
    (progn
      (vla-put-TrueColor (car pair) tc)
      (vla-put-TrueColor (cadr pair) tc)))
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

(defun ecd:follow-xref  (/ pt obj)
  (setq obj (ecd:pick-object "Выберите ссылку\n"))
  (if (and obj (ecd:in-xrefs-p obj))
    (progn
      (setq pt (ecd:get-insertion-point
                 (ecd:xref-target obj)))
      (ecd:move-view-to-point (car pt) (cadr pt)))))

(defun ecd:save-xrefs  ()
  ;;permanently save xrefs data in drawing
  (ecd:clean-xrefs)
  (vlax-ldata-put
    *ecd:dict-name*
    *ecd:xref-key*
    (mapcar '(lambda (lst)
               (mapcar 'vla-get-handle lst))
            *ecd:xrefs*)))

(defun ecd:acad-major-version ()
  (substr (vla-get-version (vlax-get-acad-object)) 1 2))

(defun ecd:i-acad-color  ()
  (if *ecd:color-interface-object*
    *ecd:color-interface-object*
    (setq *ecd:color-interface-object*
           (vla-GetInterfaceObject
             (vlax-get-acad-object)
             (strcat "AutoCAD.AcCmColor." (ecd:acad-major-version))))))
