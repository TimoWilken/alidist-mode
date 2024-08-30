(use-modules ((gnu packages emacs-xyz)
              #:select (emacs-mmm-mode emacs-yaml-mode))
             (guix build-system emacs)
             (guix gexp)
             (guix git-download)
             (guix packages)
             ((guix licenses) #:prefix license:))

(define %source-dir
  (dirname (current-filename)))

(define emacs-alidist-mode
  (package
    (name "emacs-alidist-mode")
    (version "0.0")
    (source
     (local-file %source-dir
                 #:recursive? #t
                 #:select? (git-predicate %source-dir)))
    (build-system emacs-build-system)
    ;; The custom, flymake and sh-script packages are part of Emacs.
    (propagated-inputs (list emacs-mmm-mode emacs-yaml-mode))
    (home-page "https://cgit.twilken.net/alidist-mode/about/")
    (synopsis "Emacs major mode for ALICE alidist packages.")
    (description "This Emacs major mode nicely highlights the ALICE
experiment's @url{https://github.com/alisw/alidist, alidist} recipes nicely,
using @code{yaml-mode} for their metadata headers and @code{sh-mode} for any
embedded shell scripts.

It also integrates with flymake to show messages from
@url{https://github.com/TimoWilken/alidistlint, alidistlint}.")
    (license license:gpl3+)))

emacs-alidist-mode
