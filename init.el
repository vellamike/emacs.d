(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(package-initialize)

 ;; ido mode
(ido-mode t)

 ;;column numbers:
(column-number-mode t)

;;undo tree mode
(add-hook 'after-init-hook 'global-undo-tree-mode)

;;hasekll mode
(load "~/.emacs.d/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(put 'upcase-region 'disabled nil)

;;having some trouble with fonts in terminal, choose a monospaced
(if (equal (display-graphic-p) 'false) (set-default-font "Inconsolata-12")
)

(setq initial-scratch-message "")

;;python
(add-to-list 'load-path "~/.emacs.d/pymacs") ;;mv experiment
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-1.3.1/dict")
(ac-config-default)
;;get python indentation right: - MV temporarily canceled this to see what effect would be
;;(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'ne;;wline-and-indent)))

;; enable yasnippet
(add-to-list 'load-path
              "~/.emacs.d/yasnippet") (require 'yasnippet) 
(yas-global-mode 1)

;; Auto-Save on ^Z
(add-hook 'suspend-hook 'do-auto-save)


(when (display-graphic-p)
  (defun toggle-fullscreen ()
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			   '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			   '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
    )
(toggle-fullscreen)
)

;package requirement
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;select color theme
(color-theme-initialize)
(color-theme-deep-blue)

;;Some stuff I'm not currently using but would like to keep here for reference:

;(require 'color-theme)
;;; colour themes:
;(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
;(if
;    (equal 0 (string-match "^24" emacs-version))
;    ;; it's emacs24, so use built-in theme
;    (require 'solarized-dark-theme)
;  ;; it's NOT emacs24, so use color-theme
;  (progn
;    (require 'color-theme)
;    (color-theme-initialize)
;    (require 'color-theme-solarized)
;    (color-theme-solarized-light)))
