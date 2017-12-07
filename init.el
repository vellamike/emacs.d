;;; Mike Vella's init.el file
;require common lisp
(require 'cl)

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(setq ash-packages
      '(ace-jump-mode
        anaphora
        autopair
        dart-mode
        expand-region
        fill-column-indicator
        flex-autopair
	flycheck-irony
        flymake-cursor
        flyspell-lazy
        idle-highlight-mode
        js2-mode
        key-chord
        list-utils
        magit
        oauth2
        paredit
        rainbow-delimiters
        rainbow-mode
        smex
        magit
        ido-ubiquitous
        find-file-in-project
        undo-tree
        yasnippet
	neotree
	company-jedi
	company-irony
        solarized-theme
        zenburn-theme
	auctex
	jedi
	markdown-mode
	powerline
	lorem-ipsum
	irony
	go-mode
	yaml-mode
	company-go
	handlebars-mode
	tern
	company-tern
        ))

(package-initialize)

;;; install missing packages
(let ((not-installed (remove-if 'package-installed-p ash-packages)))
  (if not-installed
      (if (y-or-n-p (format "there are %d packages to be installed. install them? "
                            (length not-installed)))
          (progn (package-refresh-contents)
                 (dolist (package not-installed)
                   (package-install package))))))


;; Javascript
;; you probably want to install tern (npm install -g tern)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js2-mode-hook '(lambda () (setq-local company-backends '((company-tern :with company-yasnippet)))))


;; GOLANG MODE configuration
;; for go mode the following is needed, or makes it much better
;; go get -u github.com/dougm/goflymake
;; go get -u github.com/nsf/gocode
;; go get github.com/rogpeppe/godef


(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
			  (company-mode)))

;;(add-to-list 'load-path "~/dev/go/src/github.com/dougm/goflymake")
;;(require 'go-flymake)

(setenv "GOPATH" (concat (getenv "HOME") "/dev/go"))
(setenv "PATH" (concat (getenv "PATH") ":~/dev/go/bin"))
;;Adding to PATH
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/usr/local/go/bin")))
(setq exec-path (append exec-path '("~/dev/go/bin")))


;;Display time and date in emacs status bar
(setq display-time-day-and-date t
   display-time-24hr-format t)
(display-time)

;;; Reverse colors for the border to have nicer line
(set-face-inverse-video-p 'vertical-border nil)
(set-face-background 'vertical-border (face-background 'default))
;
;; Set symbol for the border
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

;; Autocomplete with company mode and irony mode for C++
;; irony mode requires you to install the irony-server
(global-company-mode)
(setq company-idle-delay 0) ;; No delay
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;;auto revert mode  - if file changes (e.g by an external process) revert the buffer
(global-auto-revert-mode 1)

;;global line number mode - puts line numbers on the left
(global-linum-mode t)

;;undo-tree mode
(add-hook 'after-init-hook 'global-undo-tree-mode)

;;flycheck mode
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;;flake 8 needs to be installed via "Pip install flake8 - this gives you sophisticated python syntax checking"
(setq flycheck-flake8-maximum-line-length 120)

;;monospaced font - Inconsolata should be installed if not available
(condition-case nil
    (set-default-font "Inconsolata-12")
  (error nil))

;get rid of emacs startup message
(setq inhibit-startup-message t)

;;get rid of annoying scratch message
(setq initial-scratch-message "")

;; enable yasnippet - never really got it to work..
(yas-global-mode 1)


;; Auto-Save on ^Z
(add-hook 'suspend-hook 'do-auto-save)

;; remove scroll bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;;select color theme, also disable all themes before loading
(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(load-theme 'wombat t)

;;change cursor colour, I find it easier to spot this way
(set-cursor-color "red")

;;;;jedi -- autocomplete for Python - I use company mode not AC (the Jedi default)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;;Smex is a M-x enhancement for Emacs.
;;Built on top of IDO, it provides a convenient interface to your recently and most frequently used commands.
;;And to all the other commands, too.
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; ido-mode
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(ido-ubiquitous-mode t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode t)
  (tool-bar-mode -1)
  (menu-bar-mode -1))

;;Window manipulation
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;;linenum mode prevents doc view mode from working properly
(add-hook 'doc-view-mode-hook
  (lambda ()
    (linum-mode -1)
  ))

;thin cursor
(setq-default cursor-type 'bar)

;; can't do it at launch or emacsclient won't always honor it
(add-hook 'before-make-frame-hook 'esk-turn-off-tool-bar)

;replace yes and no with y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;fullscreen
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

;fancy status line
(powerline-default-theme)

;toggle full screen with F11 key
(global-set-key [f11] 'toggle-fullscreen)

;change window sizes using arrow keys
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)
(global-set-key [f5] 'goto-line)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "0f0e3af1ec61d04ff92f238b165dbc6d2a7b4ade7ed9812b4ce6b075e08f49fe" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(ecb-options-version "2.40")
 '(fci-rule-color "#383838")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (handlebars-mode company-go yaml-mode go-mode irony lorem-ipsum powerline markdown-mode jedi auctex zenburn-theme solarized-theme company-irony company-jedi neotree yasnippet undo-tree find-file-in-project ido-ubiquitous smex rainbow-mode rainbow-delimiters paredit oauth2 magit list-utils key-chord js2-mode idle-highlight-mode flyspell-lazy flymake-cursor flycheck-irony flex-autopair fill-column-indicator expand-region dart-mode autopair anaphora ace-jump-mode)))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)

(put 'downcase-region 'disabled nil)
