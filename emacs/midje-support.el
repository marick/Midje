(provide 'midje-support)

(defvar midje-root (expand-file-name "~"))

(defun clojure-here ()
  (interactive)
  (global-set-key "\^h\^h" 'clojure-visit-source)    
  (setq midje-root (file-name-directory buffer-file-name))
)

(defvar midje-filename-stash '())

(defun midje-reload-filename-stash (dir)
  (setq midje-filename-stash
	(split-string
	 (shell-command-to-string
	  (concat "find "
		  (shell-quote-argument dir)
		  " -name "
		  (shell-quote-argument "*.clj")
		  " -print "))))
  nil)

(defun midje-matching-file (file)
  (let* ((regexp (concat "/" file "$")))
    (find-if (lambda (fullpath) (string-match regexp fullpath))
	     midje-filename-stash))
)

(defun midje-goto (file line increment)
  (let ((relevant-file (or (midje-matching-file file)
			   (midje-reload-filename-stash midje-root)
			   (midje-matching-file file))))
    (message (concat "relevant file is " relevant-file))
    (message increment)
    (if (not relevant-file) 
	(error (concat "No file matches " file))
      (find-file-other-window relevant-file)
      (goto-line (string-to-int line))
      (if increment 
	  (search-forward "=>" nil nil (string-to-int increment)))))
)

(defun match-part (n)
  (if (match-beginning n)
      (buffer-substring (match-beginning n) (match-end n))
    nil))

(defun clojure-visit-source ()
  "If the current line contains text like '../src/program.clj:34', visit 
that file in the other window and position point on that line."
  (interactive)
  (let* ((start-boundary (save-excursion (beginning-of-line) (point)))
         (regexp (concat "\\([ \t\n\r\"'([<{]\\|^\\)" ; non file chars or
                                                      ; effective
                                                      ; beginning of file  
                         "\\(.+\\.clj\\):\\([0-9]+\\)" ; file.rb:NNN
			 "\\(\\+[0-9]\\)?"
			 )) 
         (matchp (save-excursion
                  (end-of-line)
                  ;; if two matches on line, the second is most likely
                  ;; to be useful, so search backward.
                  (re-search-backward regexp start-boundary t))))

    (if matchp
	(let ((file (match-part 2))
	      (line (match-part 3))
	      (increment (match-part 4)))
	  (midje-goto file line increment))
      (error "No Clojure location on line."))))
