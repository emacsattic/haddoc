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
  "If set, the location of `haddoc-lookup' program.")

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
	  (message "Browsing: \"%s\"" (strip-whitespace (thing-at-point 'line)))
	  (browse-url url))
      (error "No URL on this line"))))

(defvar haddoc-return-buffer nil)
;;(make-variable-buffer-local 'haddoc-return-buffer)

(defun haddoc-quit-window ()
  "Leave the completions window."
  (interactive)
  (let ((popbuf haddoc-return-buffer))
    (quit-window)
    (pop-to-buffer popbuf)))

(defun haddoc-do-lookup (search-term)
  "Runs a lookup process and returns a list of (term, url) pairs."
  (mapcar 
   (lambda (x) (split-string x ";"))
   (filter 
    (lambda (x) (> (length x) 0))
    (split-string
     (with-output-to-string
       (call-process haddoc-lookup-program nil standard-output nil search-term))
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
      (let ((data (split-string (thing-at-point 'line) ";")))
	(message "Browsing: \"%s\"" (car data))
	(browse-url (cadr data)))
      )

     ;; N. Multiple results.
     (t
      ;; Decorate the temporary buffer lines with appropriate properties for
      ;; selection.
      (let* ((curbuf (current-buffer))
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
	(set (make-local-variable 'haddoc-return-buffer) curbuf)

	))
      ))
    )


;; Filter function, in case it is not defined.
(if (not (fboundp 'filter))
    (defun filter (pred list)
      "Returns a list of all the elements fulfilling the pred requirement."
      (if list
          (let ((head (car list))
                (tail (filter pred (cdr list))))
            (if (funcall pred head)
                (cons head tail)
              tail)))))



(provide 'haddoc)
;;; haddoc.el ends here
