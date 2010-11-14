;;; midje-mode.el --- Minor mode for Midje tests
;;
;; This is a minor mode designed to be used with clojure-mode.el and slime.el

;; What's in my .emacs file:
;; (eval-after-load 'clojure-mode
;;   '(define-clojure-indent
;;      (fact 'defun)
;;      (facts 'defun)
;;      (against-background 'defun)
;;      (provided 0)))

;; (require 'clojure-mode)
;; (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;; (require 'midje-mode)
;; (add-hook 'clojure-mode-hook 'midje-mode)
;; (require 'clojure-jump-to-file)


(require 'clojure-mode)
(require 'slime)
(require 'newcomment)

(defvar midje-running-fact nil)   ;; KLUDGE!

(defvar midje-comments ";.;.")
(defvar last-checked-midje-fact nil)
(defvar midje-fact-regexp "^(facts?\\([[:space:]]\\|$\\)")
(defvar midje-syntax-table nil)

;; Callbacks
(defun midje-insert-above-fact (result)
  (if (bolp) (forward-char)) ; at first character of defun, beginning-of-defun moves back.
  (beginning-of-defun)
  (midje-provide-result-info result))

(defun midje-insert-below-code-under-test (result)
  (end-of-defun)
  (next-line)
  (midje-provide-result-info result)
)

;; Util

(defun midje-at-start-of-identifier? ()
  (not (string= (string (char-syntax (char-before))) "w")))

(defun midje-identifier ()
  "Return text of nearest identifier."
  (when (not midje-syntax-table)
    (setq midje-syntax-table (make-syntax-table (syntax-table)))
    (modify-syntax-entry ?- "w" midje-syntax-table)
    (modify-syntax-entry ?? "w" midje-syntax-table)
    (modify-syntax-entry ?! "w" midje-syntax-table))

  (save-excursion 
    (with-syntax-table midje-syntax-table
      (let ((beg (if (midje-at-start-of-identifier?)
		     (point)
		   (progn (backward-word) (point)))))
	(forward-word)
	(buffer-substring-no-properties beg (point))))))

(defun midje-to-unfinished ()
  (goto-char (point-min))
  (search-forward "(unfinished"))

(defun midje-within-unfinished? () 
  (let ((target (point))
	 unfinished-beg
	 unfinished-end)
    (save-excursion
      (save-restriction
	(midje-to-unfinished)
	(beginning-of-defun)
	(setq unfinished-beg (point))
	(end-of-defun)
	(setq unfinished-end (point))
	(and (>= target unfinished-beg)
	     (<= target unfinished-end))))))

(defun midje-tidy-unfinished ()
  (midje-to-unfinished) (let ((fill-prefix "")) (fill-paragraph nil))
  (midje-to-unfinished)
  (beginning-of-defun)
  (let ((beg (point)))
    (end-of-defun)
    (indent-region beg (point))))

(defun midje-add-identifier-to-unfinished-list (identifier)
  (save-excursion
    (save-restriction
      (widen)
      (midje-to-unfinished) (insert " ") (insert identifier)
      (slime-interactive-eval (concat "(unfinished " identifier ")"))
      (midje-tidy-unfinished))))

(defun midje-remove-identifier-from-unfinished-list ()
  (save-excursion
    (save-restriction
      (widen)
      (let ((identifier (midje-identifier)))
	(with-syntax-table midje-syntax-table
	  (unless (midje-at-start-of-identifier?) (backward-word))
	  (kill-word nil)
	  (midje-tidy-unfinished)
	  identifier)))))

(defun midje-add-defn-after-unfinished (identifier)
  (widen)
  (end-of-defun)
  (newline-and-indent)
  (insert "(defn ")
  (insert identifier)
  (insert " [])")
  (newline-and-indent)
  (newline-and-indent)
  (insert "(fact \"\")")
  (newline-and-indent)
  (search-backward "[]")
  (forward-char))

(defun midje-provide-result-info (result)
  (destructuring-bind (output value) result
    (if (string= output "")
	(midje-display-reward)
      (midje-insert-failure-message output))))

(defun midje-insert-failure-message (str &optional justify)
  (let ((start-point (point))
	(end-point (progn (insert str) (point))))
    (midje-add-midje-comments start-point end-point)
    (goto-char start-point)
    (unless (string= ";" (char-to-string (char-after))) 
      (delete-char 1))))

(defun midje-display-reward ()
  (save-excursion
    (save-restriction
      (let ((start (point)))
	(insert (midje-random-praise))
	(narrow-to-region start (point))
	(goto-char (point-min))
	(fill-paragraph nil)
	(midje-add-midje-comments (point-min) (point-max))))))

(defun midje-add-midje-comments (start-point end-point)
  (let ((comment-start midje-comments)
	(comment-empty-lines t))
    (comment-region start-point end-point)))

(defun midje-on-fact? () 
  (save-excursion
    (save-restriction
      (narrow-to-defun)
      (goto-char (point-min))
      (search-forward "fact" nil t))))

(defun midje-doto-facts (fun)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward midje-fact-regexp nil t)
      (funcall fun))))


;; Interactive

(defun midje-next-fact ()
  (interactive)
  (re-search-forward midje-fact-regexp))

(defun midje-previous-fact ()
  (interactive)
  (re-search-backward midje-fact-regexp))

(defun midje-clear-comments ()
  "Midje uses comments to display test results. Delete
all such comments."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((kill-whole-line t))
      (while (search-forward midje-comments nil t)
	(beginning-of-line)
	(kill-line)))))

(defun midje-check-fact-near-point ()
  "Used when `point' is on or just after a Midje fact.
Check that fact and also save it for use of
`midje-recheck-last-fact-checked'."
  (interactive)
  (midje-clear-comments)
  (let ((string (save-excursion
		 (mark-defun)
		 (buffer-substring-no-properties (mark) (point)))))
    (setq last-checked-midje-fact string)
    (slime-eval-async `(swank:eval-and-grab-output ,string)
      'midje-insert-above-fact)))

(defun midje-recheck-last-fact-checked ()
  "Used when `point` is on or just after a def* form.
Has the Clojure REPL compile that form, then rechecks
the last fact checked (by `midje-check-fact-near-point')."

  (interactive)
  (midje-clear-comments)
  (setq midje-running-fact t)
  (slime-compile-defun)
  ; Callback is slime-compilation-finished, then midje-after-compilation-check-fact
)

;; This is a HACK. I want to add midje-after-compilation-check-fact to
;; the slime-compilation-finished-hook, but I can't seem to override the 
;; :options declaration in the original slime.el defcustom. 
(unless (fboundp 'original-slime-compilation-finished)
  (setf (symbol-function 'original-slime-compilation-finished)
	(symbol-function 'slime-compilation-finished)))

(defun slime-compilation-finished (result)
  (original-slime-compilation-finished result)
  (with-struct (slime-compilation-result. notes duration successp) result
    (if successp (midje-after-compilation-check-fact))))

(defun midje-after-compilation-check-fact ()
  (if midje-running-fact
      (slime-eval-async `(swank:eval-and-grab-output ,last-checked-midje-fact)
	'midje-insert-below-code-under-test))
  (setq midje-running-fact nil))

(defun midje-check-fact ()
  "If on or near a Midje fact, check it with
`midje-check-fact-near-point'. Otherwise, compile the
nearby Clojure form and recheck the last fact checked
(with `midje-recheck-last-fact-checked')."
  (interactive)
  (if (midje-on-fact?) 
      (midje-check-fact-near-point)
    (midje-recheck-last-fact-checked)))

(defun midje-hide-all-facts ()
  (interactive)
  (midje-doto-facts #'hs-hide-block))

(defun midje-show-all-facts ()
  (interactive)
  (midje-doto-facts #'hs-show-block))


(defun midje-focus-on-this-fact ()
  (interactive)
  (midje-hide-all-facts)
  (hs-show-block))

(defun midje-unfinished ()
  (interactive)
  (if (midje-within-unfinished?) 
      (midje-add-defn-after-unfinished (midje-remove-identifier-from-unfinished-list))
    (midje-add-identifier-to-unfinished-list (midje-identifier))))

(defvar midje-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ,") 'midje-check-fact)
    (define-key map (kbd "C-c .") 'midje-check-fact)
    (define-key map (kbd "C-c C-,") 'midje-check-fact-near-point)
    (define-key map (kbd "C-c C-.") 'midje-recheck-last-fact-checked)
    (define-key map (kbd "C-c k")   'midje-clear-comments)

    (define-key map (kbd "C-c f") 'midje-focus-on-this-fact)
    (define-key map (kbd "C-c h") 'midje-hide-all-facts)
    (define-key map (kbd "C-c s") 'midje-show-all-facts)

    (define-key map (kbd "C-c n") 'midje-next-fact)
    (define-key map (kbd "C-c p") 'midje-previous-fact)

    (define-key map (kbd "C-c u") 'midje-unfinished)

    map)
  "Keymap for Midje mode.")

;;;###autoload
(define-minor-mode midje-mode
"A minor mode for running Midje tests when in `slime-mode'.

\\{midje-mode-map}"
  nil " Midje" midje-mode-map
  ;; This doesn't seem to work.
  ;; (custom-add-option 'slime-compilation-finished-hook
  ;;                    'midje-post-compilation-action)
  (hs-minor-mode 1))

(provide 'midje-mode)
(require 'midje-mode-praise)
