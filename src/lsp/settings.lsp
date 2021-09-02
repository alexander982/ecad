(vl-load-com)

(setq *doc* (vla-get-activeDocument (vlax-get-acad-object)))
(setq *mspace* (vla-get-modelSpace *doc*))

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

(setq *ecd:viewport-height* 150)

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

(defun ecd:save-first-page-setup  (setup)
  (vla-add *dicts* *ecd:dict-name*)
  (vlax-ldata-put *ecd:dict-name*
    *ecd:first-page-key*
    (ecd:normolize-page-setup setup)))

(defun ecd:set-pages-layout (layout)
  (setq *ecd:pages-layout* layout))

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
