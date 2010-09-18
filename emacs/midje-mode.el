;;; midje-mode.el --- Minor mode for Midje tests

(require 'clojure-mode)
(require 'slime)

(defvar midje-comments ";.;.")
(defvar last-checked-midje-fact nil)


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
    (delete-char 1)))

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


;; Interactive

(defun midje-clear-comments ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((kill-whole-line t))
      (while (search-forward midje-comments nil t)
	(beginning-of-line)
	(kill-line)))))

(defun midje-check-fact-near-point ()
  (interactive)
  (midje-clear-comments)
  (let ((string (save-excursion
		 (mark-defun)
		 (buffer-substring-no-properties (mark) (point)))))
    (setq last-checked-midje-fact string)
    (slime-eval-async `(swank:eval-and-grab-output ,string)
      'midje-insert-above-fact)))


(defun midje-recheck-last-fact-checked ()
  (interactive)
  (midje-clear-comments)
  (slime-compile-defun)
  (slime-eval-async `(swank:eval-and-grab-output ,last-checked-midje-fact)
    'midje-insert-below-code-under-test))

(defun midje-check-fact ()
  (interactive)
  (if (midje-on-fact?) 
      (midje-check-fact-near-point)
    (midje-recheck-last-fact-checked)))



(defvar midje-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ,") 'midje-check-fact)
    (define-key map (kbd "C-c C-,") 'midje-check-fact-near-point)
    (define-key map (kbd "C-c C-.") 'midje-recheck-last-fact-checked)
    (define-key map (kbd "C-c k")   'midje-clear-comments)
    map)
  "Keymap for Midje mode.")

;;;###autoload
(define-minor-mode midje-mode
  "A minor mode for running Midje tests."
  nil " Midje" midje-mode-map
  (when (slime-connected-p)
    (run-hooks 'slime-connected-hook)))

(provide 'midje-mode)
(require 'midje-mode-praise)
