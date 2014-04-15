;require common lisp
(require 'cl)

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("technomancy" . "http://repo.technomancy.us/emacs/")))

(setq ash-packages
      '(ace-jump-mode
        anaphora
        autopair
        cppcheck
        dart-mode
        expand-region
        fill-column-indicator
        flex-autopair
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
        python-mode
        magit
        ido-ubiquitous
        find-file-in-project
        undo-tree
        yasnippet
        pymacs
        auto-complete
        solarized-theme
        zenburn-theme
	auctex
	auto-complete-auctex
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


;;; Reverse colors for the border to have nicer line  
(set-face-inverse-video-p 'vertical-border nil)
(set-face-background 'vertical-border (face-background 'default))
;
;; Set symbol for the border
(set-display-table-slot standard-display-table
                        'vertical-border 
                        (make-glyph-code ?┃))



;;global line number mode - puts line numbers on the left
(global-linum-mode t)

;;undo-tree mode
(add-hook 'after-init-hook 'global-undo-tree-mode) 

;;monospaced font - Inconsolata should be installed if not available
(set-default-font "Inconsolata-12")

;get rid of emacs startup message
(setq inhibit-startup-message t)

;;get rid of annoying scratch message
(setq initial-scratch-message "")

;;auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'latex-mode)

;; enable yasnippet
(yas-global-mode 1)

;; Auto-Save on ^Z
(add-hook 'suspend-hook 'do-auto-save)

;; remove scroll bars
(scroll-bar-mode -1)

;fullscreen - disabled atm
;(defun toggle-fullscreen ()
;  (interactive)
;  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;)
;(if (eq window-system 'X) (toggle-fullscreen))

;;select color theme
(load-theme 'wombat t)

;;python-mode indentation
;;this is a matter of taste, when I hit ENTER and I'm in a method I
;;want the level of indentation to be preserved rather than having to
;;hit TAB agian.
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\r" 'newline-and-indent)))


;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

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

;thin cursor
(setq-default cursor-type 'bar) 

;; can't do it at launch or emacsclient won't always honor it
(add-hook 'before-make-frame-hook 'esk-turn-off-tool-bar)

;replace yes and no with y and n
(defalias 'yes-or-no-p 'y-or-n-p)


;change window sizes
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(custom-safe-themes (quote ("fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "0f0e3af1ec61d04ff92f238b165dbc6d2a7b4ade7ed9812b4ce6b075e08f49fe" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#383838")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50) ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85) ("#073642" . 100))))
 '(magit-diff-use-overlays nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list (quote (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
