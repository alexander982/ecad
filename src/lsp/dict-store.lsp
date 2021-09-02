(vl-load-com)

(setq *dicts* (vla-get-dictionaries *doc*))

(setq *ecd:dict-name* "ecad")
(setq *ecd:dict* (vla-add *dicts* *ecd:dict-name*))
(setq *ecd:first-page-key* "1st-page")
(setq *ecd:xref-key* "xref")
(setq *ecd:page-setup-key* "page-setup")

