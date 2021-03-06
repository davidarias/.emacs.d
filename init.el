;;; darthcoder-emacs-config --- My Emacs Config
;;; Commentary:
;;; Header to avoid flycheck warnings
;;; Code:

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
                      js2-mode
                      rjsx-mode
                      tern
                      tern-auto-complete
                      auto-complete
                      python-mode
                      pyvenv
                      py-autopep8
                      clojure-mode
                      smex
                      lua-mode
                      hemisu-theme
                      atom-dark-theme
                      base16-theme
                      spacemacs-theme
                      spaceline
                      doom-themes
                      nlinum
                      flx-ido
                      ido-vertical-mode
                      jedi
                      web-mode
                      neotree
                      all-the-icons
                      projectile
                      flycheck
                      dashboard
                      highlight-indent-guides
                      key-seq
                      eyebrowse
                      markdown-mode
                      dumb-jump
                      use-package
                      highlight-symbol))

; install the missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; write Custom autogenerated config in a differnt file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; maximized
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; enable projectile globaly
;; init projectile before dashboard to show projects list
(require 'projectile)
(projectile-global-mode)


;; workaround bug
;; https://github.com/rakanalh/emacs-dashboard/issues/45
(require 'use-package)

(use-package dashboard
  :bind (
	 :map dashboard-mode-map
	 ("<down-mouse-1>" . nil)
	 ("<mouse-1>" . widget-button-click)
	 ("<mouse-2>" . widget-button-click))
  :config
  (setq dashboard-banner-logo-title "May the source be with you")
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 10)))
  :init
  (dashboard-setup-startup-hook))

;; (require 'dashboard)
;; (dashboard-setup-startup-hook)

;; (setq dashboard-banner-logo-title "May the source be with you")
;; (setq dashboard-items '((recents  . 5)
;;                         (bookmarks . 5)
;;                         (projects . 5)))


(require 'better-defaults)

;; org mode babel src blocks behaviour
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-support-shift-select t)

;; enable support for languages in org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (emacs-lisp . t)
   (js . t)
   (python . t)
   (sh . t )))

;; flycheck babel src code blocks
(defadvice org-edit-src-code (around set-buffer-file-name activate compile)
  (let ((file-name (buffer-file-name))) ;; (1)
    ad-do-it                            ;; (2)
    (setq buffer-file-name file-name))) ;; (3)

;; scroll one line at a time
(setq mouse-wheel-scroll-amount '(4))
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

 ;; keyboard scroll one line at a time
(setq scroll-step 1)

;; allow use accented chars
(require 'iso-transl)

;; do not ask follow symblinks
(setq vc-follow-symlinks nil)


;; move between windows using meta + arrows
(windmove-default-keybindings 'meta)

;; overwrite selected text
(delete-selection-mode 1)

;; some anarchist developers use tabs instead of spaces
(setq-default tab-width 4)

;; c/c++ indent
(setq-default c-basic-offset 4)

;; no anoying visible bell
(setq visible-bell nil)

(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers

;; show line numbers
;; (global-linum-mode t)
;; (add-hook 'term-mode-hook (lambda () (linum-mode 0)))


;; better performance linum
(require 'nlinum)
(add-hook 'prog-mode-hook 'nlinum-mode)

;; activated in better defaults
;; (require 'ido)
;; (ido-mode t)

(ido-mode 1)
(ido-everywhere 1)

(ido-vertical-mode 1)
;; move ido vertical with up and down arrows
;; and use left right for history
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(flx-ido-mode 1)
(setq ido-auto-merge-work-directories-length -1)

(eyebrowse-mode t)
;; use alternative keybindings
(eyebrowse-setup-opinionated-keys)

(require 'spaceline-config)
(spaceline-spacemacs-theme)
;; hide minor modes ( they take too much space )
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-buffer-size-off)
(setq powerline-default-separator 'wave )
(spaceline-compile)


(require 'highlight-symbol)
(global-set-key (kbd "C-ñ") 'highlight-symbol-at-point)
(global-set-key (kbd "C-.") 'highlight-symbol-next)
(global-set-key (kbd "C-,") 'highlight-symbol-prev)
(global-set-key (kbd "C-;") 'highlight-symbol-query-replace)

;; for neo tree icons
;; run after M-x all-the-icons-install-fonts
(require 'all-the-icons)

;; neo tree config
(require 'neotree)
;; (global-set-key [f8] 'neotree-toggle)
;; (setq neo-theme 'nerd)
;; use doom theme for neotree
(doom-themes-neotree-config)

;; allow resize neotree window
(setq neo-window-fixed-size nil)

(defun neotree-project-dir ()
  "Open NeoTree using the git root if in project"
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (if (projectile-project-p)
        (let ((project-dir (projectile-project-root))
              (file-name (buffer-file-name)))
          (if project-dir
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name))
            (message "Could not find git project root.")))
      (neotree-toggle))))

(global-set-key [f8] 'neotree-project-dir)


;; set color themes ( requires emacs-goodies package)
;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-charcoal-black)

;; (set-frame-font "Hermit 10" nil t)

(load-theme 'doom-molokai t)

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

;; ido theme tweaks
;; (custom-set-faces
;;  '(ido-subdir ((t (:foreground "#999")))) ;; Face used by ido for highlighting subdirs in the alternatives.
;;  '(ido-first-match ((t (:foreground "#fff")))) ;; Face used by ido for highlighting first match.
;;  '(ido-only-match ((t (:foreground "#fff"))))) ;; Face used by ido for highlighting only match.

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'column)

;; enable flycheck
(global-flycheck-mode)

;; jump to definitions
(dumb-jump-mode)

;;; auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; avoid linum weird behaviour
(ac-linum-workaround)

;; use markdown mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; use js2 mode in js files
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; use rjsx mode (js2-mode plus react )
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; enable tern for autocomplete
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'rjsx-mode-hook (lambda () (progn
                                       (tern-mode t)
                                       (auto-complete-mode))))

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;; use ac-js2
;; (add-hook 'js2-mode-hook 'ac-js2-setup-auto-complete-mode)
;; (setq ac-js2-evaluate-calls t)

;; globals for js2-mode
(setq js2-global-externs (list "window" "define" "require" "module" "exports"))

;; use 4 spaces in html mode
(add-hook 'html-mode-hook
	  (lambda ()
	    ;; Default indentation is usually 2 spaces, changing to 4.
	    (set (make-local-variable 'sgml-basic-offset) 4)))


;; web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;; use python-mode
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(add-hook 'python-mode-hook 'jedi:setup)

;; use python mode for Scons build files
 (setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
 (setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))

;; (setq py-shell-name "ipython")

;; autopep8 on save
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; smalltalk mode
(add-to-list 'load-path "~/.emacs.d/lisp")
(setq auto-mode-alist
      (append  '(("\\.st\\'" . smalltalk-mode))
               auto-mode-alist))

(autoload 'smalltalk-mode "smalltalk-mode.el" "Smalltalk mode" t)

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

;; delete trailing space before save
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

;; Key bindings

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(global-set-key (kbd "C-S-d") 'duplicate-line)



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

(global-set-key (kbd "C-p") 'projectile-find-file)
(global-set-key (kbd "C-S-p") 'projectile-switch-project)

;; key-seq bindings
;; key-seq provides a way to map pairs of sequentially
;; but quickly pressed keys to commands
;;(key-chord-mode 1)

;; examples:
;; (key-seq-define-global "qd" 'dired)
;; (key-seq-define text-mode-map "qf" 'flyspell-buffer)

;; (key-seq-define-global "pf" 'projectile-find-file)
;; (key-seq-define-global "pp" 'projectile-switch-project)
