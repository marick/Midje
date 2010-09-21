
;;; midje-mode.el --- Minor mode for Midje tests
;;
;; I use these indentation settings for the two main Midje constructs:
;;
;; (eval-after-load 'clojure-mode
;;   '(define-clojure-indent
;;      (fact 'defun)
;;      (provided 0)))

(require 'clojure-mode)
(require 'slime)

(defvar midje-comments ";.;.")
(defvar last-checked-midje-fact nil)
(defvar midje-fact-regexp "^(facts?\\([[:space:]]\\|$\\)")

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
  (slime-eval-async `(swank:eval-and-grab-output ,last-checked-midje-fact)
    'midje-insert-below-code-under-test))


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

(defvar midje-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ,") 'midje-check-fact)
    (define-key map (kbd "C-c C-,") 'midje-check-fact-near-point)
    (define-key map (kbd "C-c C-.") 'midje-recheck-last-fact-checked)
    (define-key map (kbd "C-c k")   'midje-clear-comments)

    (define-key map (kbd "C-c f") 'midje-focus-on-this-fact)
    (define-key map (kbd "C-c h") 'midje-hide-all-facts)
    (define-key map (kbd "C-c s") 'midje-show-all-facts)

    (define-key map (kbd "C-c n") 'midje-next-fact)
    (define-key map (kbd "C-c p") 'midje-previous-fact)
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
