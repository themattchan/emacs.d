(setq gnus-select-method
      '(nnimap "awakesecurity"
               (nnimap-address "outlook.office365.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnir-search-engine imap)
               (nnmail-expiry-wait 90)))

(setq smtpmail-smtp-server "smtp.office365.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(add-to-list 'gnus-secondary-select-methods
             '(nntp "news.gmane.org"))

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-thread-sort-functions
	  '(gnus-thread-sort-by-most-recent-date
		gnus-thread-sort-by-most-recent-number
		gnus-thread-sort-by-subject))
