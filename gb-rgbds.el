;;; gb-rgbds.el --- Game Boy RGBDS helpers -*- lexical-binding: t; -*-

;; Author: piratalpha
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (mwim "0.9"))
;; Keywords: tools, gameboy, rgbds
;; URL: https://github.com/piratalpha/gb-rgbds

;;; Commentary:
;;
;; Simple Game Boy RGBDS build + run helpers.
;;
;; Provides commands:
;; - gb-rgbds-build
;; - gb-rgbds-run
;; - gb-rgbds-build-and-run
;;

(add-to-list 'load-path
             (expand-file-name "lisp"
                               (file-name-directory load-file-name)))

(require 'gb-rgbds-mode)

;;; Code:

(defgroup gb-rgbds nil
  "Game Boy RGBDS workflow helpers."
  :group 'tools)

(defcustom gb-rgbds-main-file "main.asm"
  "Main ASM file for the Game Boy project."
  :type 'string)

(defcustom gb-rgbds-emulicious-jar
  "~/tools/emulicious/emulicious.jar"
  "Path to Emulicious.jar."
  :type 'file
  :group 'gb-rgbds)

;;;###autoload
(defun gb-rgbds--project-root ()
  "Return the project root containing `gb-rgbds-main-file`."
  (or (locate-dominating-file default-directory gb-rgbds-main-file)
      (error "No %s found in any parent directory" gb-rgbds-main-file)))

;;;###autoload
(defun gb-rgbds-build ()
  "Build the Game Boy ROM using RGBDS."
  (interactive)
  (let* ((root (gb-rgbds--project-root))
         (default-directory root)
         (cmd
          (format
           "rgbasm -o main.o %s && rgblink -o main.gb main.o && rgbfix -v -p 0 main.gb"
           gb-rgbds-main-file))
         (compilation-buffer-name-function
          (lambda (_) "*gb-rgbds-build*")))
    (compile cmd)))

;;;###autoload
(defun gb-rgbds-run ()
  "Run the built ROM in Emulicious."
  (interactive)
  (unless gb-rgbds-emulicious-jar
    (error "gb-rgbds-emulicious-jar is not set"))
  (let* ((root (gb-rgbds--project-root))
         (rom (expand-file-name "main.gb" root)))
    (unless (file-exists-p rom)
      (error "ROM not found, build first"))
    (start-process
     "emulicious"
     "*gb-rgbds-emulicious*"
     "java" "-jar" gb-rgbds-emulicious-jar rom)))

;;;###autoload
(defun gb-rgbds-build-and-run ()
  "Build and run the ROM."
  (interactive)
  (let ((hook nil))
    (setq hook
          (lambda (_buf status)
            (when (string-match-p "finished" status)
              (gb-rgbds-run))
            (remove-hook 'compilation-finish-functions hook)))
    (add-hook 'compilation-finish-functions hook)
    (gb-rgbds-build)))

(provide 'gb-rgbds)
;;; gb-rgbds.el ends here
