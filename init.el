;; remove tools bar
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; how line numbers
(global-linum-mode t)

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

(add-hook 'focus-out-hook 'save-all)

;; package manager
(require 'package)

;; set the list of packaes to install
;; py-autopep8 requires the autopep8 tool (pip install autopep8 )
(setq package-list '(autopair ac-js2 js2-mode auto-complete yasnippet python-mode py-autopep8))

;; packages repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers

(require 'ido)
(ido-mode t)

;; set color themes ( requires emacs-goodies package)
(require 'color-theme)
(color-theme-initialize)
(color-theme-charcoal-black)

;;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;; auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; use js2 mode in js files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; globals for js2-mode
(setq js2-global-externs (list "window" "define"))

;; use 4 spaces in html mode
(add-hook 'html-mode-hook
	  (lambda ()
	    ;; Default indentation is usually 2 spaces, changing to 4.
	    (set (make-local-variable 'sgml-basic-offset) 4)))

;; use python-mode
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; autopep8 on save
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)


;; Key bindings
(global-set-key (kbd "C-x TAB") 'indent-region)
