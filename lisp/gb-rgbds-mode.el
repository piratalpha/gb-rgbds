;;; gb-rgbds-mode.el --- RGBDS major mode for Game Boy assembly -*- lexical-binding: t; -*-

;; Derived from rgbds-mode by japanoise
;; https://github.com/japanoise/rgbds-mode
;;
;; Original work © japanoise and contributors
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Major mode for Game Boy RGBDS assembly.
;;; Based on https://www.cemetech.net/forum/viewtopic.php?t=6413&start=0

;;; Code:

(require 'mwim)

(defconst gb-rgbds-instructions
  '("adc" "add" "and" "cp" "dec" "inc" "or" "sbc" "sub" "xor"
    "call" "jp" "jr" "ret" "rst"
    "bit" "res" "rl" "rlc" "rr" "rrc" "set" "sla" "sra" "srl" "swap"
    "ccf" "cpl" "daa" "di" "ei" "halt" "nop" "scf" "stop"
    "ld" "pop" "push" "reti" "rla" "rlca" "rra" "rrca"))

(defconst gb-rgbds-directives
  '("db" "dw" "ds" "section" "equ" "equs" "set"
    "macro" "endm" "rsset" "rsreset" "rb" "rw"
    "export" "global" "incbin" "include"
    "if" "elif" "else" "endc"
    "rept" "endr" "fail" "warn" "charmap"))

(defconst gb-rgbds-font-lock-keywords
  `(
    ;; Comments
    (";.*" . font-lock-comment-face)

    ;; Labels
    ("^[[:space:]]*\\([[:word:]_]+:\\)" 1 font-lock-function-name-face)

    ;; Instructions
    (,(regexp-opt gb-rgbds-instructions 'symbols) . font-lock-builtin-face)

    ;; Directives
    (,(regexp-opt gb-rgbds-directives 'symbols) . font-lock-preprocessor-face)

    ;; Registers
    ("\\_<\\(a\\|b\\|c\\|d\\|e\\|h\\|l\\|af\\|bc\\|de\\|hl\\|sp\\)\\_>"
     . font-lock-variable-name-face)

    ;; Numbers
    ("\\_<\\(0x[0-9a-fA-F]+\\|\\$[0-9a-fA-F]+\\|%[01]+\\|[0-9]+\\)\\_>"
     . font-lock-constant-face)
    ))

(define-derived-mode gb-rgbds-mode prog-mode "GB-RGBDS"
  "Major mode for Game Boy RGBDS assembly."
  (setq-local font-lock-defaults '(gb-rgbds-font-lock-keywords))
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil))

(define-key gb-rgbds-mode-map (kbd "C-j") #'newline-and-indent)
(define-key gb-rgbds-mode-map (kbd "RET") #'newline-and-indent)
(define-key gb-rgbds-mode-map (kbd "C-a") #'mwim-beginning-of-code-or-line)
(define-key gb-rgbds-mode-map (kbd "C-e") #'mwim-end-of-code-or-line)

(provide 'gb-rgbds-mode)
;;; gb-rgbds-mode.el ends here
