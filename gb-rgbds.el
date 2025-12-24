;;; gb-rgbds.el --- Game Boy RGBDS helpers -*- lexical-binding: t; -*-

;; Author: piratalpha
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (mwim "0.9"))
;; Keywords: tools, gameboy, rgbds
;; URL: https://github.com/piratalpha/gb-rgbds

;;; Commentary:
;;
;; Game Boy RGBDS workflow helpers.
;; Includes commands to build, run, and debug ROMs.
;;

(require 'gb-rgbds-mode)
(require 'compile)

;;; Code:

(defgroup gb-rgbds nil
  "Game Boy RGBDS workflow helpers."
  :group 'tools)

;; -- Configuration Variables --

(defcustom gb-rgbds-main-asm-file "main.asm"
  "The entry point assembly file for the project."
  :type 'string
  :safe #'stringp
  :group 'gb-rgbds)

(defcustom gb-rgbds-build-command
  "rgbasm -o %s.o %s && rgblink -n %s.sym -o %s.gb %s.o && rgbfix -v -p 0 %s.gb"
  "Command string to build the project.
The %s placeholders are: (1) base name, (2) source file, (3) base name, 
(4) base name, (5) base name, (6) base name."
  :type 'string
  :safe #'stringp
  :group 'gb-rgbds)

(defcustom gb-rgbds-emulicious-jar nil
  "Path to the Emulicious.jar file."
  :type 'file
  :group 'gb-rgbds)

;; -- Internal Helpers --

(defun gb-rgbds--project-root ()
  "Find the directory containing the main asm file."
  (or (locate-dominating-file default-directory gb-rgbds-main-asm-file)
      (error "Could not find %s in directory hierarchy." gb-rgbds-main-asm-file)))

(defun gb-rgbds--get-basename (filename)
  "Get the filename without extension (e.g., 'main.asm' -> 'main')."
  (file-name-sans-extension filename))

;; -- Public Commands --

;;;###autoload
(defun gb-rgbds-build ()
  "Compile the current project using RGBDS."
  (interactive)
  (let* ((root (gb-rgbds--project-root))
         (source gb-rgbds-main-asm-file)
         (base (gb-rgbds--get-basename source))
         ;; Fill the command with the base name and source name
         (command (format gb-rgbds-build-command base source base base base base)))
    (compile command)))

;;;###autoload
(defun gb-rgbds-run ()
  "Run the built ROM in Emulicious."
  (interactive)
  (let* ((root (gb-rgbds--project-root))
         (base (gb-rgbds--get-basename gb-rgbds-main-asm-file))
         (rom-path (expand-file-name (concat base ".gb") root))
         (jar-path (expand-file-name gb-rgbds-emulicious-jar)))
    (unless (file-exists-p rom-path)
      (user-error "ROM not found: %s. Build it first!" rom-path))
    (message "Starting Emulicious with %s..." (file-name-nondirectory rom-path))
    (call-process "java" nil 0 nil "-jar" jar-path rom-path)))

;;;###autoload
(defun gb-rgbds-build-and-run ()
  "Build the project and then run it."
  (interactive)
  ;; 1. Capture the CURRENT buffer's setting (e.g., "test.asm")
  ;;    We act as a bridge, carrying this value to the compilation process.
  (let ((correct-asm-file gb-rgbds-main-asm-file)
        (hook-fn nil)) ;; Initialize the variable first to avoid the "void" error
    
    ;; 2. Define the hook function
    (setq hook-fn 
          (lambda (buf str)
            ;; Check if compilation succeeded
            (if (null (string-match "abnormally" str))
                (progn
                  ;; 3. Temporarily force the variable to be what we captured
                  ;;    This fixes the "Could not find main.asm" error.
                  (let ((gb-rgbds-main-asm-file correct-asm-file))
                    (gb-rgbds-run))
                  
                  ;; Clean up the hook so it doesn't run again later
                  (remove-hook 'compilation-finish-functions hook-fn))
              
              ;; If failed, just clean up
              (message "Build failed; skipping run.")
              (remove-hook 'compilation-finish-functions hook-fn))))

    ;; Add the hook and start building
    (add-hook 'compilation-finish-functions hook-fn)
    (gb-rgbds-build)))

;;;###autoload
(defun gb-rgbds-clean ()
  "Remove generated build files (.o, .gb, .sym)."
  (interactive)
  (let* ((root (gb-rgbds--project-root))
         (files '("*.o" "*.gb" "*.sym")))
    (dolist (pattern files)
      (let ((matched-files (file-expand-wildcards (expand-file-name pattern root))))
        (dolist (file matched-files)
          (delete-file file)
          (message "Deleted: %s" (file-name-nondirectory file))))))
  (message "Cleanup complete."))

(provide 'gb-rgbds)
;;; gb-rgbds.el ends here
