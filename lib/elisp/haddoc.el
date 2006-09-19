;;; haddoc.el --- Browse Python HTML documentation from Emacs

;; Copyright 2006 by Martin Blais.

;; Authors: Martin Blais <blais@furius.ca>,
;; Revision: $Revision: 4509 $
;; Date: $Date: 2006-04-19 19:21:21 -0400 (Wed, 19 Apr 2006) $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 2,
;; as published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License version 2
;; along with this program and available at
;; http://docutils.sf.net/licenses/gpl.txt and at
;; http://www.gnu.org/licenses/gpl.txt.

;;; Commentary:

;; From a database of parsed Python HTML indexes, queries that database using an
;; external tool (haddoc-lookup) and directs a web browser to the selected page.

;; The database can be constructed via emacs:
;; (setq haddoc-db-file "~/haddoc.db")
;; M-x haddoc-update

;; Bind the `haddoc-lookup' function to some key and enter the desired search
;; terms.  I use this::
;;
;;     (define-key py-mode-map [(control c)(?.)] 'haddoc-lookup)
;;

;;; Description

;; This a tool for Emacs users writing Python code, who like to use Python HTML
;; documentation rather than the texinfo.  It saves you time by making it
;; possible to search the index and direct a web browser directly to the
;; appropriate web page from within Emacs.

;;; History:

;;; Code:

(require 'browse-url)

(defvar haddoc-lookup-program "haddoc-lookup"
  "The location of `haddoc-lookup' program.")

(defvar haddoc-update-program "haddoc-update"
  "The location of `haddoc-update' program.")

(defvar haddoc-db-file "/var/lib/haddoc/haddoc.db"
  "The location for the haddoc database file.")

(defvar haddoc-documentation-locations
  '("file:///usr/share/doc/python/html" "http://python.org/doc/html")
  "A list of possible python documentation locations")

(defvar haddoc-history nil)

(defvar haddoc-temp-buffer-name "*Haddoc Completions*")

(defvar haddoc-mode-map
  (let ((map (make-sparse-keymap)))
;;    (define-key map [mouse-1] 'haddoc-mode-mouse-goto-kill)
;;    (define-key map [mouse-2] 'haddoc-mode-mouse-goto)
    (define-key map "\C-m" 'haddoc-mode-lookup-and-leave)
    (define-key map "f" 'haddoc-mode-lookup)
    (define-key map "q" 'haddoc-quit-window)
    (define-key map "z" 'kill-this-buffer)
    map)
  "Keymap for `haddoc-mode-mode'.")

(put 'haddoc-mode 'mode-class 'special)

(defun haddoc-mode ()
  "Major mode for output from \\[haddoc-lookup]."
  (interactive)
  (kill-all-local-variables)
  (use-local-map haddoc-mode-map)
  (setq major-mode 'haddoc-mode)
  (setq mode-name "Haddoc")
  (setq buffer-read-only t)
  )

(defun haddoc-mode-lookup-and-leave ()
  "Lookup the current line in a browser and leave the completions window."
  (interactive)
  (call-interactively 'haddoc-mode-lookup)
  (haddoc-quit-window))

(defun haddoc-mode-lookup ()
  "Lookup the current line in a browser."
  (interactive)
  (let ((url (get-text-property (point) 'haddoc-target-url)))
    (if url
	(progn
	  (beginning-of-line)
	  (message "Browsing: \"%s\"" (haddoc-strip-whitespace (thing-at-point 'line)))
	  (browse-url (haddoc-normalize-url url)))
      (error "No URL on this line"))))

(defvar haddoc-return-window-config nil)
;;(make-variable-buffer-local 'haddoc-return-window-config)

(defun haddoc-quit-window ()
  "Leave the completions window."
  (interactive)
  (set-window-configuration haddoc-return-window-config))

(defun haddoc-do-lookup (search-term)
  "Runs a lookup process and returns a list of (term, url) pairs."
  (mapcar 
   (lambda (x) (split-string x ";"))
   (haddoc-filter 
    (lambda (x) (> (length x) 0))
    (split-string
     (with-output-to-string
       (call-process haddoc-lookup-program nil standard-output nil 
		     "-D" (expand-file-name haddoc-db-file) search-term))
     "\n"))))

(defun haddoc-complete (search-term pred tr)
  "Completion function for haddoc."
  (mapcar 'car (haddoc-do-lookup search-term)))

(defun haddoc-lookup (search-term)
  "Lookup SEARCH-TERM in the Python HTML indexes."
  (interactive
   (list 
    (let ((initial (thing-at-point 'word)))
      (completing-read  "Lookup Python documentation for: "
			'haddoc-complete
			nil nil initial 'haddoc-history))
    ))

  (let ((matches (haddoc-do-lookup search-term)))
    (cond

     ;; 0. No results.
     ((eq matches nil)
      (message "No matches for \"%s\"." search-term))

     ;; 1. A single result.
     ((= (length matches) 1)  
      ;; Point the browser at the unique result and get rid of the buffer
      (let ((data (car matches)))
	(message "Browsing: \"%s\"" (car data))
	(browse-url (haddoc-normalize-url (cadr data))))
      )

     ;; N. Multiple results.
     (t
      ;; Decorate the temporary buffer lines with appropriate properties for
      ;; selection.
      (let* ((cur-window-conf (current-window-configuration))
	     (tmpbuf (get-buffer-create haddoc-temp-buffer-name)))
    
	(display-buffer tmpbuf)
	(pop-to-buffer tmpbuf)

	(setq buffer-read-only nil)
	(erase-buffer)

	;; Insert the text in the buffer
	(insert (format "Python index matches for %s:\n\n" search-term))
	(mapcar 
	 (lambda (x) 
	   (insert (car x))
	   (put-text-property
	    (line-beginning-position) (line-end-position)
	    'haddoc-target-url (cadr x))
	   (insert "\n"))
	 matches)

	(goto-line 3)
	
	(haddoc-mode)
	(set (make-local-variable 'haddoc-return-window-config) cur-window-conf)

	))
      ))
    )

(defun haddoc-normalize-url (path)
  "Make sure that the given path is a URL."
  (if (or (string-match "^/" path)
	  (string-match "^[a-z]:" path))
      (concat "file://" path)
    path))

;;; Generic functions missing from Emacs:

(defun haddoc-filter (pred list)
  "Returns a list of all the elements fulfilling the pred requirement."
  (if list
      (let ((head (car list))
	    (tail (haddoc-filter pred (cdr list))))
	(if (funcall pred head)
	    (cons head tail)
	  tail))))

(defun haddoc-strip-whitespace (str)
  "Strips the whitespace around a string."
  (let ((tmp))
    (string-match "\\`[ \t\n]*" str)
    (setq tmp (substring str (match-end 0)))
    (string-match "[ \t\n]*\\'" tmp)
    (substring tmp 0 (match-beginning 0))
    ))

(defun haddoc-update (src)
  "Run haddoc-update and create the database at `haddoc-db-file'."
  (interactive (list (completing-read "Python Html Documentation source: "
                                      haddoc-documentation-locations)))
  ;; haddoc-update -D /home/myuser/.haddoc/haddoc.db <URL>
  (shell-command (concat haddoc-update-program " -D "
                         (expand-file-name haddoc-db-file) " " src)))



(provide 'haddoc)
;;; haddoc.el ends here
