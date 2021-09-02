(vl-load-com)

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