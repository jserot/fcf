;;; fcf-mode.el --- simple major mode for editing FCF code (.fcf). -*- coding: utf-8; lexical-binding: t; -*-
;; Inspired by http://ergoemacs.org/emacs/elisp_syntax_coloring.html
;; For doc : https://www.emacswiki.org/emacs/EmacsSyntaxTable

(require 'cc-langs)

(setq fcf-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("type" "fsm" "let" "in" "and" "const" "var"))
            (x-types '("int" "signed" "unsigned" "bool" "float" "array"))
            (x-constants '("true" "false"))
            (x-functions '("return" "do" "then"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp (regexp-opt x-types 'words))
            (x-constants-regexp (regexp-opt x-constants 'words))
            (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face)
          ;;(,x-events-regexp . font-lock-builtin-face)
          (,x-functions-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

(defvar fcf-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?- ". 12b" table)
    (modify-syntax-entry ?' "." table)  ;; "'" is for denoting type variables, not for enclosing char constants
    table)
  "Syntax table for `fcf-mode'.")

(setq fcf-font-lock-keywords
    (append fcf-font-lock-keywords
    '(("->\\||" . font-lock-function-name-face))))

(define-derived-mode fcf-mode c-mode "fcf mode"
  "Major mode for editing FCF programs"
  :syntax-table fcf-syntax-table
  (setq comment-start "-- ")
  (setq comment-end "")
  (setq font-lock-defaults '((fcf-font-lock-keywords))))

(provide 'fcf-mode)
