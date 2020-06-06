;; -*- coding: utf-8 -*-
;(defvar best-gc-cons-threshold gc-cons-threshold "Best default gc threshold value. Should't be too big.")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar best-gc-cons-threshold 4000000 "Best default gc threshold value. Should't be too big.")
;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)

(setq emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *emacs24* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 24))) )
(setq *no-memory* (cond
                   (*is-a-mac*
                    (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                   (*linux* nil)
                   (t nil)))

(setq *emacs24old*  (or (and (= emacs-major-version 24) (= emacs-minor-version 3))
                        (not *emacs24*)))

;; *Message* buffer should be writable in 24.4+
(defadvice switch-to-buffer (after switch-to-buffer-after-hack activate)
  (if (string= "*Messages*" (buffer-name))
      (read-only-mode -1)))

;; @see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
(let ((file-name-handler-alist nil))
  (require 'init-autoload)
  (require 'init-modeline)
  ;; (require 'cl-lib) ; it's built in since Emacs v24.3
  (require 'init-compat)
  (require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
  (require 'init-utils)

  ;; Windows configuration, assuming that cygwin is installed at "c:/cygwin"
  ;; (condition-case nil
  ;;     (when *win64*
  ;;       ;; (setq cygwin-mount-cygwin-bin-directory "c:/cygwin/bin")
  ;;       (setq cygwin-mount-cygwin-bin-directory "c:/cygwin64/bin")
  ;;       (require 'setup-cygwin)
  ;;       ;; better to set HOME env in GUI
  ;;       ;; (setenv "HOME" "c:/cygwin/home/someuser")
  ;;       )
  ;;   (error
  ;;    (message "setup-cygwin failed, continue anyway")
  ;;    ))

  (require 'idle-require)
  (require 'init-elpa)
  (require 'init-exec-path) ;; Set up $PATH
  ;; any file use flyspell should be initialized after init-spelling.el
  ;; actually, I don't know which major-mode use flyspell.
  (require 'init-spelling)
  (require 'init-gui-frames)
  (require 'init-ido)
  (require 'init-dired)
  (require 'init-uniquify)
  (require 'init-ibuffer)
  (require 'init-ivy)
  (require 'init-hippie-expand)
  (require 'init-windows)
  (require 'init-sessions)
  (require 'init-git)

  (require 'init-markdown)
  (require 'init-erlang)
  (require 'init-javascript)
  (require 'init-org)
  (require 'init-css)
  (require 'init-python-mode)
  (require 'init-haskell)
  (require 'init-ruby-mode)
  (require 'init-lisp)
  (require 'init-elisp)
  (require 'init-yasnippet)
  ;; Use bookmark instead
  (require 'init-cc-mode)
  (require 'init-gud)
  (require 'init-linum-mode)
  ;; (require 'init-gist)
  (require 'init-moz)
  (require 'init-gtags)
  ;; init-evil dependent on init-clipboard
  (require 'init-clipboard)
  ;; use evil mode (vi key binding)
  ;;(require 'init-evil)
  (require 'init-multiple-cursors)
  (require 'init-sh)
  (require 'init-ctags)
  (require 'init-bbdb)
  (require 'init-gnus)
  (require 'init-lua-mode)
  (require 'init-workgroups2)
  (require 'init-term-mode)
  (require 'init-web-mode)
  (require 'init-slime)
  (require 'init-company)
  ;;(Require 'init-chinese-pyim) ;; cannot be idle-required
  ;; need statistics of keyfreq asap
;;  (require 'init-keyfreq)
  (require 'init-httpd)

  ;; default size
  (custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
  ;;default coding
   (setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
  (set-language-environment 'utf-8)
  (set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (unless (eq system-type 'windows-nt)
   (set-selection-coding-system 'utf-8))
  (prefer-coding-system 'utf-8)
  ;;cppcm
(setq cppcm-get-executable-full-path-callback
      (lambda (path type tgt-name)
        ;; path is the supposed-to-be target's full path
        ;; type is either add_executabe or add_library
        ;; tgt-name is the target to built. The target's file extension is stripped
        (message "cppcm-get-executable-full-path-callback called => %s %s %s" path type tgt-name)
        (let* ((dir (file-name-directory path))
               (file (file-name-nondirectory path)))
          (cond
           ((string= type "add_executable")
            (setq path (concat dir "bin/" file)))
           ;; for add_library
           (t (setq path (concat dir "lib/" file)))
           ))
        ;; return the new path
        (message "cppcm-get-executable-full-path-callback called => path=%s" path)
        path))
(setq cppcm-write-flymake-makefile nil)
;;cscope
(require 'xcscope)
(cscope-setup)
(define-key global-map [(f12)] 'cscope-find-this-symbol)

(define-key global-map [(f6)] 'cscope-find-functions-calling-this-function)
(define-key global-map [(f11)]  'cscope-history-forward-line)
;;(define-key global-map [(f10)] 'cscope-history-forward-file)
(define-key global-map [(shift f11)] 'cscope-history-backward-line)
;;(define-key global-map [(control f10)] 'cscope-history-backward-file)
;;(define-key global-map [(f7)] 'cmake-project-configure-project)
;; misc has some crucial tools I need immediately
(require 'init-misc)

;; comment below line if you want to setup color theme in your own way
;;(if (or (display-graphic-p) (string-match-p "256color"(getenv "TERM"))) (require 'init-color-theme))
(load-theme 'zenburn t)

(require 'init-emacs-w3m)
(require 'init-hydra)
;; show the full file path
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
;; {{ idle require other stuff
(setq idle-require-idle-delay 2)
(setq idle-require-symbols '(init-perforce
                             init-misc-lazy
                             init-which-func
                             init-fonts
                             init-hs-minor-mode
                             init-writting
                             init-pomodoro
                             init-artbollocks-mode
                             init-semantic))
(idle-require-mode 1) ;; starts loading
;; }}

(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds."
           (time-to-seconds (time-since emacs-load-start-time))))

;; my personal setup, other major-mode specific setup need it.
;; It's dependent on init-site-lisp.el
(if (file-exists-p "~/.custom.el") (load-file "~/.custom.el")))

;; @see https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

(setq gc-cons-threshold best-gc-cons-threshold)
;;; Local Variables:
;;; no-byte-compile: t
;;; End:
(put 'erase-buffer 'disabled nil)

;; indicate plantuml.jar
(setq plantuml-jar-path "/playpen/software/plantuml/plantuml.jar")
;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; which function
(add-hook 'c-mode-common-hook 
  (lambda ()
    (which-function-mode t)))
(add-hook 'c++-mode-common-hook 
  (lambda ()
    (which-function-mode t)))

;; w3m
(add-to-list 'exec-path  "/usr/bin/w3m")

;; auto save
(setq auto-save-interval 500)

;; indent
(setq c-default-style "linux"
      c-basic-offset 4)

;; fold-this
    (global-set-key (kbd "C-c C-F") 'fold-this-all)
    (global-set-key (kbd "C-c C-f") 'fold-this)
(global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)


;; undo-tree
(global-undo-tree-mode 1)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "<f3>") 'next-error)
; from enberg on #emacs, hide compile window if succeed
(setq compilation-finish-function
  (lambda (buf str)
    (if (null (string-match ".*exited abnormally.*" str))
        ;;no errors, make the compilation window go away in a few seconds
        (progn
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!")))))

(setq python-shell-completion-native-enable nil)

;; matlab mode-line
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list
    'auto-mode-alist
      '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")

(setq visible-bell t)
