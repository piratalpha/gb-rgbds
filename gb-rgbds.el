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

(defcustom gb-rgbds-output-rom-name "main.gb"
  "The name of the resulting ROM file."
  :type 'string
  :safe #'stringp
  :group 'gb-rgbds)

(defcustom gb-rgbds-build-command
  "rgbasm -o main.o %s && rgblink -n main.sym -o %s main.o && rgbfix -v -p 0 %s"
  "Command string to build the project. 
%s placeholders are: source file, output rom, output rom."
  :type 'string
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

;; -- Public Commands --

;;;###autoload
(defun gb-rgbds-build ()
  "Build the Game Boy ROM using the defined build command."
  (interactive)
  (let* ((root (gb-rgbds--project-root))
         (default-directory root)) ; Execute command in project root
    (compile (format gb-rgbds-build-command 
                     gb-rgbds-main-asm-file
                     gb-rgbds-output-rom-name
                     gb-rgbds-output-rom-name))))

;;;###autoload
(defun gb-rgbds-run ()
  "Run the built ROM in Emulicious."
  (interactive)
  (let* ((root (gb-rgbds--project-root))
         (rom-path (expand-file-name gb-rgbds-output-rom-name root))
         ;; Expand the jar path to handle the "~" symbol correctly
         (jar-path (expand-file-name gb-rgbds-emulicious-jar)))
    
    ;; Sanity Checks
    (unless gb-rgbds-emulicious-jar
      (user-error "Variable `gb-rgbds-emulicious-jar' is not set"))
    (unless (file-exists-p jar-path)
      (user-error "Emulicious jar not found at: %s" jar-path))
    (unless (file-exists-p rom-path)
      (user-error "ROM file not found: %s. Build it first!" rom-path))

    (message "Starting Emulicious...")
    
    ;; We use 'call-process' with '0' as the destination.
    ;; This starts the process asynchronously and DISOWNS it, 
    ;; so Emacs doesn't wait for it to finish or get stuck.
    (call-process "java" nil 0 nil 
                  "-jar" jar-path rom-path)))

;;;###autoload
(defun gb-rgbds-build-and-run ()
  "Build the project, and if successful, run the emulator."
  (interactive)
  ;; We hook into the compilation finish process
  (let ((finish-hook nil))
    (setq finish-hook
          (lambda (buffer status)
            ;; Check if compilation finished successfully (no "exited abnormally")
            (if (string-match-p "^finished" status)
                (gb-rgbds-run)
              (message "Build failed, not running emulator."))
            ;; Cleanup the hook so it doesn't run on future compilations
            (remove-hook 'compilation-finish-functions finish-hook)))
    
    (add-hook 'compilation-finish-functions finish-hook)
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
