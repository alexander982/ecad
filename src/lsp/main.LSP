(vl-load-com)

(setq *doc* (vla-get-activeDocument (vlax-get-acad-object)))
(setq *dicts* (vla-get-dictionaries *doc*))

(setq *mspace* (vla-get-modelSpace *doc*))

(setq *ecd:color-interface-object* nil)

(setq *ecd:xrefs* '())
(setq *ecd:settings-present* nil)

;;(width height left-margin right-margin top-margin bottom-margin)
(setq *ecd:a4-book* (list 210.0 297.0 20.0 5.0 5.0 5.0))
(setq *ecd:a4-album* (list 297.0 210.0 20.0 5.0 5.0 5.0))
(setq *ecd:a3-album* (list 420.0 297.0 20.0 5.0 5.0 5.0))
(setq *ecd:page* *ecd:a4-book*)
(setq *ecd:horizontal-zones* 6)
(setq *ecd:pages-layout* "table") ;;table or row
(setq *ecd:pages-in-row* 4)
(setq *ecd:xref-color* acYellow)  ;;default color for new cross-referencies
(setq *ecd:xref-height* 3.0)      ;;default text height
(setq +ecd:color-list+ (list "Жёлтый" "Красный" "Зелёный" "Синий" "Голубой" "Фиолетовый" "Белый"))
(setq +ecd:color-indexes+ (list acYellow acRed acGreen acBlue acCyan acMagenta acWhite))

(setq *ecd:dict-name* "ecad")
(setq *ecd:dict* (vla-add *dicts* *ecd:dict-name*))
(setq *ecd:first-page-key* "1st-page")
(setq *ecd:xref-key* "xref")
(setq *ecd:page-setup-key* "page-setup")


(defun ecd:get-first-page  (/ pt1 pt2)
  (setq pt1 (getpoint "Укажите угол первого листа >_"))
  (setq pt2 (getcorner pt1 "Укажите противоположный угол >_"))
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

(defun ecd:make-2d-point  (x y)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray vlax-vbDouble '(0 . 1))
      (list x y))))

(defun ecd:move-view-to-point (x y / view)
  (setq view (vla-get-ActiveViewport *doc*))
  (vla-put-center view (ecd:make-2d-point x y))
  (vla-put-ActiveViewport *doc* view))

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

(defun ecd:follow-xref  (/ pt obj)
  (setq obj (ecd:pick-object "Выберите ссылку\n"))
  (if (and obj (ecd:in-xrefs-p obj))
    (progn
      (setq pt (ecd:get-insertion-point
                 (ecd:xref-target obj)))
      (ecd:move-view-to-point (car pt) (cadr pt)))))

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

(defun ecd:xrefs-set-color (color)
  (setq tc (ecd:i-acad-color))
  (vla-put-ColorIndex tc color)
  (foreach pair *ecd:xrefs*
    (progn
      (vla-put-TrueColor (car pair) tc)
      (vla-put-TrueColor (cadr pair) tc)))
  (princ))

(defun ecd:acad-major-version ()
  (substr (vla-get-version (vlax-get-acad-object)) 1 2))

(defun ecd:i-acad-color  ()
  (if *ecd:color-interface-object*
    *ecd:color-interface-object*
    (setq *ecd:color-interface-object*
           (vla-GetInterfaceObject
             (vlax-get-acad-object)
             (strcat "AutoCAD.AcCmColor." (ecd:acad-major-version))))))

(defun ecd:setup-page-dialog
                              (/ page-data page-format horizontal-zones
                               pages-layout pages-in-row
                               pick-first-page dcl-id xref-color xref-height)
  (if (setq page-data (vlax-ldata-get *ecd:dict-name* *ecd:page-setup-key*))
    (setq page-format      (cadr (assoc "page-format" page-data))
          horizontal-zones (cadr (assoc "horizontal-zones" page-data))
          pages-layout      (cadr (assoc "pages-layout" page-data))
          pages-in-row     (cadr (assoc "pages-in-row" page-data))
          xref-color       (cadr (assoc "xref-color" page-data))
          xref-height      (cadr (assoc "xref-height" page-data)))
    ;; defaults
    (setq page-format
           "0"
          horizontal-zones
           "6"
          pages-layout
           "table"
          pages-in-row "4"
          xref-color "0"
          xref-height "3.0"))
  (if (vlax-ldata-get *ecd:dict-name* *ecd:first-page-key*)
    (setq pick-first-page "0")
    (setq pick-first-page "1"))
  (if (< (setq dcl-id (load_dialog "page_setup.dcl")) 0)
    (progn
      (alert "диалог page_setup.dcl не найден")
      (exit)))
  (if (not (new_dialog "page_setup" dcl-id))
    (progn
      (alert "ошибка создания диалога")
      (exit)))
  (start_list "pageFormat")
  (mapcar 'add_list (list "A4 книжн." "А3 альб."))
  (end_list)
  (if (= pages-layout "table")
    (set_tile "listInTable" "1")
    (progn
      (set_tile "listInRow" "1")
      (mode_tile "qntListsInRow" 1)))
  (set_tile "xrSize" xref-height)
  (start_list "xrColor")
  (mapcar 'add_list +ecd:color-list+)
  (end_list)
  (set_tile "xrColor" xref-color)
  (set_tile "pickFirstPage" pick-first-page)
  (set_tile "qntListsInRow" pages-in-row)
  (set_tile "hZones" horizontal-zones)
  (set_tile "pageFormat" page-format)
  (action_tile "pageFormat" "(setq page-format $value)")
  (action_tile "xrColor" "(setq xref-color $value)")
  (action_tile
    "listInTable"
    "(setq pages-layout \"table\") (mode_tile \"qntListsInRow\" 0)")
  (action_tile
    "listInRow"
    "(setq pages-layout \"row\") (mode_tile \"qntListsInRow\" 1)")
  (action_tile "pickFirstPage" "(setq pick-first-page $value)")
  (action_tile "accept"
    (strcat "(setq pages-in-row (get_tile \"qntListsInRow\"))"
            "(setq horizontal-zones (get_tile \"hZones\"))"
            "(setq xref-height (get_tile \"xrSize\"))"
            "(done_dialog 1)"))
  (start_dialog)
  (unload_dialog dcl-id)
  ;;TODO add user input validation
  (setq page-data (list (list "page-format" page-format)
                        (list "horizontal-zones" horizontal-zones)
                        (list "pages-layout" pages-layout)
                        (list "pages-in-row" pages-in-row)
                        (list "xref-color" xref-color)
                        (list "xref-height" xref-height)))
  (if (= pick-first-page "1")
    (ecd:save-first-page-setup (ecd:get-first-page)))
  (setq *ecd:pages-layout*  pages-layout
        *ecd:pages-in-row*  (atoi pages-in-row)
        *ecd:horizontal-zones*  (atoi horizontal-zones)
        *ecd:xref-height* (atof xref-height)
        *ecd:xref-color* (nth (atoi xref-color) +ecd:color-indexes+)
        *ecd:page*  (cond
                      ((= page-format "0") *ecd:a4-book*)
                      ((= page-format "1") *ecd:a3-album*))
        *ecd:settings-present* t)
  (vlax-ldata-put *ecd:dict-name* *ecd:page-setup-key* page-data))

(defun ecd:load-page-setup  (/ page-data page-format)
  (if (setq page-data (vlax-ldata-get *ecd:dict-name* *ecd:page-setup-key*))
    (setq
      page-format
       (cadr (assoc "page-format" page-data))
      *ecd:pages-layout*
       (cadr (assoc "pages-layout" page-data))
      *ecd:pages-in-row*
       (atoi (cadr (assoc "pages-in-row" page-data)))
      *ecd:horizontal-zones*
       (atoi (cadr (assoc "horizontal-zones" page-data)))
      *ecd:page*
       (cond
         ((= page-format "0")
          *ecd:a4-book*)
         ((= page-format "1")
          *ecd:a3-album*))
      *ecd:xref-color* (nth (atoi (cadr (assoc "xref-color" page-data)))
                            +ecd:color-indexes+)
      *ecd:xref-height* (atof (cadr (assoc "xref-height" page-data)))
      *ecd:settings-present* t)))

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

(princ (strcat "Найдено " (itoa (length *ecd:xrefs*)) " пар ссылок\n"))

;|«Visual LISP© Format Options»
(80 2 40 0 nil "end of " 80 9 0 0 1 nil T T T)
;*** DO NOT add text below the comment! ***|;
