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
;; Includes robust syntax highlighting and compilation helpers.

;;; Code:

(require 'mwim)

(defconst gb-rgbds-font-lock-keywords-1
  (list
   ;; Comments
   '(";.*" . font-lock-comment-face)
   '("^\\*.*" . font-lock-comment-face)
   
   ;; Instructions (both cases)
   '("\\<\\(adc\\|add\\|and\\|cp\\|dec\\|inc\\|or\\|sbc\\|sub\\|xor\\|call\\|jp\\|jr\\|ret\\|rst\\|bit\\|res\\|rl\\|rlc\\|rr\\|rrc\\|set\\|sla\\|sra\\|srl\\|swap\\|ccf\\|cpl\\|daa\\|di\\|ei\\|halt\\|nop\\|scf\\|stop\\|ld\\|pop\\|push\\|reti\\|rla\\|rlca\\|rra\\|rrca\\)\\>" . font-lock-builtin-face)
   '("\\<\\(ADC\\|ADD\\|AND\\|CP\\|DEC\\|INC\\|OR\\|SBC\\|SUB\\|XOR\\|CALL\\|JP\\|JR\\|RET\\|RST\\|BIT\\|RES\\|RL\\|RLC\\|RR\\|RRC\\|SET\\|SLA\\|SRA\\|SRL\\|SWAP\\|CCF\\|CPL\\|DAA\\|DI\\|EI\\|HALT\\|NOP\\|SCF\\|STOP\\|LD\\|POP\\|PUSH\\|RETI\\|RLA\\|RLCA\\|RRA\\|RRCA\\)\\>" . font-lock-builtin-face)
   
   ;; Registers
   '("\\<\\(a\\|b\\|c\\|d\\|e\\|h\\|l\\|af\\|bc\\|de\\|hl\\|sp\\|A\\|B\\|C\\|D\\|E\\|H\\|L\\|AF\\|BC\\|DE\\|HL\\|SP\\)\\>" . font-lock-variable-name-face)
   
   ;; Labels (ending with :)
   '("\\(\\w*:\\)"  . font-lock-function-name-face))
  "Minimal highlighting expressions for rgbds mode.")

(defconst gb-rgbds-font-lock-keywords-2
  (append gb-rgbds-font-lock-keywords-1
          (list
           ;; Constants / Numbers (Hex, Bin, Dec)
           '("\\<\\(\\([0-9][0-9A-Fa-f]*[Hh]\\|\\(0[Xx]\\|[0-9]\\|\\$[0-9A-Fa-f]\\)[0-9A-Fa-f]*\\)\\|[01][01]*[Bb]\\|%[01][01]*\\|[0-9]*\\)\\>" . font-lock-constant-face)
           ;; Immediate value symbol ($)
           '("\\(\\$\\)" . font-lock-warning-face)))
  "Additional Keywords to highlight in rgbds mode.")

(defconst gb-rgbds-font-lock-keywords-3
  (append gb-rgbds-font-lock-keywords-2
          (list
           ;; Local labels / Preprocessor symbols (.label, #macro_arg)
           '("\\(\\.\\w*\\|#\\w*\\)" . font-lock-preprocessor-face)
           
           ;; Preprocessor Directives (UPPERCASE)
           '("\\<\\(DB\\|DW\\|DS\\|SECTION\\|EQU\\|EQUS\\|SET\\|POPS\\|PUSHS\\|MACRO\\|ENDM\\|RSSET\\|RSRESET\\|RB\\|RW\\|SHIFT\\|EXPORT\\|GLOBAL\\|PURGE\\|INCBIN\\|UNION\\|NEXTU\\|ENDU\\|PRINTT\\|PRINTI\\|PRINTV\\|PRINTF\\|REPT\\|ENDR\\|FAIL\\|WARN\\|INCLUDE\\|IF\\|ELIF\\|ELSE\\|ENDC\\|CHARMAP\\)\\>" . font-lock-keyword-face)
           
           ;; Preprocessor Directives (lowercase)
           '("\\<\\(db\\|dw\\|ds\\|section\\|equ\\|equs\\|set\\|pops\\|pushs\\|macro\\|endm\\|rsset\\|rsreset\\|rb\\|rw\\|shift\\|export\\|global\\|purge\\|incbin\\|union\\|nextu\\|endu\\|printt\\|printi\\|printv\\|printf\\|rept\\|endr\\|fail\\|warn\\|include\\|if\\|elif\\|else\\|endc\\|charmap\\)\\>" . font-lock-keyword-face)))
  "Full highlighting for rgbds mode.")

(defvar gb-rgbds-font-lock-keywords gb-rgbds-font-lock-keywords-3
  "Default highlighting expressions for rgbds mode.")

;;;###autoload
(define-derived-mode gb-rgbds-mode prog-mode "GB-RGBDS"
  "Major mode for Game Boy RGBDS assembly."
  
  ;; Set up font locking
  (setq-local font-lock-defaults '(gb-rgbds-font-lock-keywords))
  
  ;; Comment settings
  (setq-local comment-start ";")
  (setq-local comment-end "")
  
  ;; Indentation
  (setq-local indent-tabs-mode nil)
  
  ;; Default Compile Command (Your custom addition)
  (setq-local compile-command 
              (format "rgbasm -o main.o %s && rgblink -o main.gb main.o && rgbfix -v -p 0 main.gb" "main.asm")))

;;; Syntax Table Definition
;; This must be defined separately or attached to the mode symbol.
;; define-derived-mode automatically creates `gb-rgbds-mode-syntax-table`.
(defvar gb-rgbds-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)   ; Underscore is part of a word
    (modify-syntax-entry ?# "w" st)   ; Hash is part of a word (macros)
    (modify-syntax-entry ?. "w" st)   ; Dot is part of a word (local labels)
    (modify-syntax-entry ?\; "<" st)  ; Semi-colon starts comment
    (modify-syntax-entry ?\n ">" st)  ; Newline ends comment
    (modify-syntax-entry ?\t "-" st)  ; Tab is whitespace
    st)
  "Syntax table for gb-rgbds-mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.asm\\'" . gb-rgbds-mode))

;; Keybindings
(define-key gb-rgbds-mode-map (kbd "C-j") #'newline-and-indent)
(define-key gb-rgbds-mode-map (kbd "RET") #'newline-and-indent)
(define-key gb-rgbds-mode-map (kbd "C-a") #'mwim-beginning-of-code-or-line)
(define-key gb-rgbds-mode-map (kbd "C-e") #'mwim-end-of-code-or-line)

(provide 'gb-rgbds-mode)
;;; gb-rgbds-mode.el ends here
