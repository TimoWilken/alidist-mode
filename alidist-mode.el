;;; alidist-mode.el --- Major mode for alidist recipes
;;; Commentary:
;;; alidist recipes are shell scripts with a YAML header in front. We want both
;;; these parts highlighted properly, and to lint the whole thing with a custom
;;; script that glues together yamllint and shellcheck with a few custom checks.
;;;
;;; This package provides a flycheck linter if flycheck is used, and/or a
;;; flymake one if that package is used. The respective checkers are only
;;; defined when their "parent" packages are loaded, if ever.
;;; Code:

(require 'sh-script)
(require 'mmm-auto)


;; Flycheck checker, to be defined when/if flycheck is loaded.

(with-eval-after-load 'flycheck
  (eval-when-compile (require 'flycheck))   ; avoid byte-compiler warnings

  (flycheck-def-executable-var alidist "alidistlint")

  (flycheck-define-checker alidist
    "A syntax checker and linter for alidist recipes."
    ;; `flycheck-alidist-executable' automatically overrides the car of the
    ;; :command list if set and non-nil.
    :command ("alidistlint" "--format=gcc" source)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": error: " (message)
            " [" (id (minimal-match (one-or-more not-newline))) "]" line-end)
     (warning line-start (file-name) ":" line ":" column ": warning: " (message)
              " [" (id (minimal-match (one-or-more not-newline))) "]" line-end)
     (info line-start (file-name) ":" line ":" column ": note: " (message)
           " [" (id (minimal-match (one-or-more not-newline))) "]" line-end))
    :modes (alidist-mode alidist-script-mode))

  (add-to-list 'flycheck-checkers 'alidist))


;; Flymake checker, to be defined when/if flymake is loaded.

(with-eval-after-load 'flymake
  (eval-when-compile (require 'flymake))   ; avoid byte-compiler warnings
  (warn "TODO: alidist-mode flymake checker"))


;; The major mode itself

(defvar-local alidist-mode-mmm-refresh-timer nil
  "An idle timer for the current buffer, to make `mmm-mode' reparse it.")
(put 'alidist-mode-mmm-refresh-timer 'risky-local-variable t)

(defun alidist-mode-cancel-refresh-timer ()
  "Cancel and delete the timer that reparses the buffer.
It is stored in `alidist-mode-mmm-refresh-timer'."
  (when alidist-mode-mmm-refresh-timer
    (cancel-timer alidist-mode-mmm-refresh-timer)
    (setq alidist-mode-mmm-refresh-timer nil)))

(define-derived-mode alidist-mode yaml-mode "alidist"
  "An outer mode for alidist recipes, handling the metadata."
  (mmm-mode)
  ;; `mmm-mode' doesn't refresh its submodes when the buffer changes (e.g. when
  ;; a *_recipe key is added to the YAML header), so refresh manually when idle.
  (alidist-mode-cancel-refresh-timer)
  (add-hook 'kill-buffer-hook #'alidist-mode-cancel-refresh-timer 0 t)
  (setq alidist-mode-mmm-refresh-timer
        (run-with-idle-timer
         2 t (lambda (original-buffer)
               (when (eq original-buffer (current-buffer))
                 ;; Silence `mmm-parse-buffer''s annoying message.
                 (let ((inhibit-message t))
                   (mmm-parse-buffer))))
         ;; Idle timers are global, so make sure we only run the timer in the
         ;; right buffer. Save the buffer now to enable this, and compare every
         ;; time the timer ticks over.
         (current-buffer))))

(define-derived-mode alidist-script-mode sh-mode "Script"
  "A mode for scripts in alidist recipes with some default settings."
  (sh-set-shell "bash"))

(mmm-add-group 'alidist-recipe
  `((alidist-main-script
     :submode alidist-script-mode
     :face mmm-default-submode-face
     :front ,(rx line-start "---\n")
     :back ,(rx buffer-end))
    (alidist-option-script
     :submode alidist-script-mode
     :face mmm-default-submode-face
     ;; Any *_recipe key with a multiline string value is probably a script.
     :front ,(rx line-start (* whitespace) (1+ (any alnum ?\_))
                 (or "_recipe" "_check") ": |\n")
     ;; End of YAML header, or another YAML key.
     :back ,(rx line-start
                (or "---\n"
                    (seq (* whitespace) (1+ (any alnum ?\_)) ":"
                         (or line-end whitespace)))))))

;; Make `mmm-mode' remember `sh-mode' indentation variables.
(cl-dolist (var sh-var-list)
  (cl-pushnew `(,var nil (sh-mode))
              mmm-save-local-variables :test 'equal))

(mmm-add-mode-ext-class 'alidist-mode nil 'alidist-recipe)
(add-to-list 'auto-mode-alist
             (cons (rx (or bot "/") "alidist/" (1+ (not ?\/)) ".sh" eot)
                   #'alidist-mode))

(provide 'alidist-mode)
;;; alidist-mode.el ends here
