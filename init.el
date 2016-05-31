;; package manager
(require 'package)

;; packages repositories

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; set the list of packaes to install
;; py-autopep8 requires the autopep8 tool (pip install autopep8 )
(defvar my-packages '(better-defaults
		      autopair
		      ac-js2
		      js2-mode
		      auto-complete
		      python-mode
                      pyvenv
		      py-autopep8
		      clojure-mode
                      smex
                      lua-mode
                      atom-dark-theme
                      base16-theme))

; install the missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))


(require 'better-defaults)

;; some anarchist developers use tabs instead of spaces
(setq-default tab-width 4)

;; c/c++ indent
(setq-default c-basic-offset 4)

;; no anoying visible bell
(setq visible-bell nil)

(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers

;; show line numbers
(global-linum-mode t)
(add-hook 'term-mode-hook (lambda () (linum-mode 0)))
;; activated in better defaults
;; (require 'ido)
;; (ido-mode t)

;; set color themes ( requires emacs-goodies package)
;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-charcoal-black)

(load-theme 'base16-ashes-dark t)

;; set fringes to background color
(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

;; set linum to background color
(set-face-attribute 'linum nil
                    ;; :foreground (face-foreground 'default)
                    :background (face-background 'default))

;; set vertical separator to background ( kind of hide )
(set-face-attribute 'vertical-border
                    nil
                    :foreground (face-background 'default))


;;; auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; avoid linum weird behaviour
(ac-linum-workaround)

;; use markdown mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; use js2 mode in js files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; globals for js2-mode
(setq js2-global-externs (list "window" "define" "require" "module" "exports"))

;; use 4 spaces in html mode
(add-hook 'html-mode-hook
	  (lambda ()
	    ;; Default indentation is usually 2 spaces, changing to 4.
	    (set (make-local-variable 'sgml-basic-offset) 4)))

;; use python-mode
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; (setq py-shell-name "ipython")

;; autopep8 on save
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; No splash screen
(setq inhibit-startup-message t)



;; hightlight matching parens
(setq show-paren-delay 0) ;; remove anoying delay
;; (show-paren-mode 1) ;; enabled in better defatuls

;; no anoying backup files
(setq make-backup-files nil)

;; dont wrap long line
(set-default 'truncate-lines t)

;;automatic autosaves anoying #files# in tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; save all buffer on autosave
(defun full-auto-save ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
	  (basic-save-buffer)))))
(add-hook 'auto-save-hook 'full-auto-save)

;; save on focus lost
(defun save-all ()
  (interactive)
  (save-some-buffers t))

;; autoreload buffers if the file is modified by an external program (i.e. git)
(global-auto-revert-mode t)

(add-hook 'focus-out-hook 'save-all)

;; Key bindings
(global-set-key (kbd "C-x TAB") 'indent-region)

(require 'smex)
;; key bindings for smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))
