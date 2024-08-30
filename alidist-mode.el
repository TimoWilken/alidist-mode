;;; alidist-mode.el --- Major mode for alidist recipes  -*- lexical-binding: t -*-
;;; Commentary:
;;; alidist recipes are shell scripts with a YAML header in front.  We
;;; want both these parts highlighted properly, and to lint the whole
;;; thing with a custom script that glues together yamllint and
;;; shellcheck with a few custom checks.
;;; Code:

(require 'custom)
(require 'flymake)
(require 'mmm-mode)
(require 'mmm-cmds)
(require 'mmm-vars)
(require 'sh-script)
(require 'yaml-mode)

(defgroup alidist-mode nil
  "Alidist-related options."
  :group 'languages
  :prefix "alidist-mode-")

(defcustom alidist-mode-alidistlint-executable "alidistlint"
  "The alidistlint executable to use.  This will be looked up in $PATH."
  :type '(string)
  :risky t
  :group 'alidist-mode)

(defvar alidist-mode--message-regexp
  (rx bol "<stdin>:"                              ; filename
      (group (+ digit)) ":"                       ; line
      (group (+ digit)) ": "                      ; column
      (group (or "note" "warning" "error")) ": "  ; type
      (group (+ not-newline)) eol)                ; message
  "Regular expression matching messages from alidistlint.
`alidist-flymake' expects the following capturing groups in this
regexp: (1) line number; (2) column number; (3) error type; (4)
message.")

(defvar-local alidist-mode--flymake-proc nil
  "The latest invocation of alidistlint.")

;; See info node: (flymake)An annotated example backend.
(defun alidist-flymake (report-fn &rest _args)
  "Run alidistlint and report diagnostics from it using REPORT-FN.
Any running invocations are killed before running another one."
  (unless (executable-find alidist-mode-alidistlint-executable)
    (funcall report-fn :panic
             :explanation "Cannot find `alidist-mode-alidistlint-executable' program")
    (error "Cannot find alidistlint executable"))

  ;; Kill previous check, if it's still running.
  (when (process-live-p alidist-mode--flymake-proc)
    (kill-process alidist-mode--flymake-proc))

  ;; This needs `lexical-binding'.
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq alidist-mode--flymake-proc
            (make-process
             :name "alidistlint-flymake" :noquery t :connection-type 'pipe
             ;; Direct output to a temporary buffer.
             :buffer (generate-new-buffer " *alidistlint-flymake*")
             :command (list alidist-mode-alidistlint-executable "-f" "gcc" "-")
             :sentinel
             (lambda (proc _event)
               "Parse diagnostic messages once the process PROC has exited."
               ;; Check the process has actually exited, not just been suspended.
               (when (memq (process-status proc) '(exit signal))
                 (unwind-protect
                     ;; Only proceed if we've got the "latest" process.
                     (if (with-current-buffer source (eq proc alidist-mode--flymake-proc))
                         (with-current-buffer (process-buffer proc)
                           (goto-char (point-min))
                           (cl-loop
                            while (search-forward-regexp alidist-mode--message-regexp nil t)
                            for (beg . end) = (flymake-diag-region
                                               source
                                               (string-to-number (match-string 1))
                                               (string-to-number (match-string 2)))
                            for type = (pcase (match-string 3)
                                         ("note" :note)
                                         ("warning" :warning)
                                         ("error" :error)
                                         (type (error "Unknown alidistlint error type %s" type)))
                            collect (flymake-make-diagnostic source beg end type (match-string 4))
                            into diags
                            finally (funcall report-fn diags)))
                       (flymake-log :warning "Canceling obsolete check %s" proc))
                   ;; Clean up temporary buffer.
                   (kill-buffer (process-buffer proc)))))))
      ;; Send the buffer to alidistlint on stdin.
      (process-send-region alidist-mode--flymake-proc (point-min) (point-max))
      (process-send-eof alidist-mode--flymake-proc))))

(defvar-local alidist-mode--mmm-refresh-timer nil
  "An idle timer for the current buffer, to make `mmm-mode' reparse it.")
(put 'alidist-mode--mmm-refresh-timer 'risky-local-variable t)

(defun alidist-mode--cancel-refresh-timer ()
  "Cancel and delete the timer that reparses the buffer.
It is stored in `alidist-mode--mmm-refresh-timer'."
  (when alidist-mode--mmm-refresh-timer
    (cancel-timer alidist-mode--mmm-refresh-timer)
    (setq alidist-mode--mmm-refresh-timer nil)))

(define-derived-mode alidist-mode yaml-mode "alidist"
  "An outer mode for alidist recipes, handling the metadata."
  (mmm-mode)
  ;; `mmm-mode' doesn't refresh its submodes when the buffer changes
  ;; (e.g. when a *_recipe key is added to the YAML header), so
  ;; refresh manually when idle.
  (alidist-mode--cancel-refresh-timer)
  (add-hook 'kill-buffer-hook #'alidist-mode--cancel-refresh-timer 0 t)
  (setq alidist-mode--mmm-refresh-timer
        (run-with-idle-timer
         2 t (lambda (original-buffer)
               (when (eq original-buffer (current-buffer))
                 ;; Silence `mmm-parse-buffer''s annoying message.
                 (let ((inhibit-message t))
                   (mmm-parse-buffer))))
         ;; Idle timers are global, so make sure we only run the timer
         ;; in the right buffer.  Save the buffer now to enable this,
         ;; and compare every time the timer ticks over.
         (current-buffer)))
  ;; Set up `flymake-mode'.
  (add-hook 'flymake-diagnostic-functions #'alidist-flymake nil t)
  (flymake-mode))

(define-derived-mode alidist-script-ts-mode bash-ts-mode "Script"
  "A mode for scripts in alidist recipes, using tree-sitter.")

(define-derived-mode alidist-script-mode sh-mode "Script"
  "A mode for scripts in alidist recipes with some default settings."
  (sh-set-shell "bash"))

(mmm-add-group
 'alidist-recipe
 `((alidist-main-script
    :submode alidist-script-mode
    :face mmm-default-submode-face
    :front ,(rx line-start "---\n")
    :back ,(rx buffer-end))
   (alidist-option-script
    :submode alidist-script-mode
    :face mmm-default-submode-face
    ;; Any *_recipe key with a multiline string value is probably a script.
    :front ,(rx line-start (* whitespace)
                (or "recipe"   ; for recipes under prefer_system_replacement_specs
                    (seq (1+ (any alnum ?\_))
                         (or "_recipe" "_check")))
                ": |\n")
    ;; End of YAML header, or another YAML key.
    :back ,(rx line-start
               (or "---\n"
                   (seq (* whitespace) (+ (any alnum ?\_)) ":"
                        (or line-end whitespace)))))))

;; Make `mmm-mode' remember `sh-mode'/`bash-ts-mode' indentation variables.
(cl-dolist (var sh-var-list)
  (cl-pushnew `(,var region (sh-mode bash-ts-mode))
              mmm-save-local-variables :test 'equal))

(mmm-add-mode-ext-class 'alidist-mode nil 'alidist-recipe)
(add-to-list 'auto-mode-alist
             (cons (rx (or bot "/") "alidist/" (1+ (not ?\/)) ".sh" eot)
                   #'alidist-mode))

(provide 'alidist-mode)
;;; alidist-mode.el ends here
