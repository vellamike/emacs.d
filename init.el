;require common lisp
(require 'cl)

(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("technomancy" . "http://repo.technomancy.us/emacs/")))

(setq ash-packages
      '(ace-jump-mode
        anaphora
        autopair
        bang
        cppcheck
        dart-mode
        dynamic-fonts
        expand-region
        fill-column-indicator
        flex-autopair
        flymake-cursor
        flyspell-lazy
        font-utils
        idle-highlight-mode
        ido-ubiquitous
        jabber
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
        starter-kit
        undo-tree
        yasnippet
        pymacs
        auto-complete
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


;;global line number mode - puts line numbers on the left
(global-linum-mode t)


;;undo-tree mode
(add-hook 'after-init-hook 'global-undo-tree-mode) 

;;monospaced font - Inconsolata should be installed if not available
(set-default-font "Inconsolata-13")

;;get rid of annoying scratch message
(setq initial-scratch-message "")

;;auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; enable yasnippet
(yas-global-mode 1)

;; Auto-Save on ^Z
(add-hook 'suspend-hook 'do-auto-save)

;;fullscreen
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)

(if (eq window-system 'X) (toggle-fullscreen))

;;select color theme
(load-theme 'wombat t)

;;python-mode indentation
;;this is a matter of taste, when I hit ENTER and I'm in a method I
;;want the level of indentation to be preserved rather than having to
;;hit TAB agian.
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\r" 'newline-and-indent)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("0f0e3af1ec61d04ff92f238b165dbc6d2a7b4ade7ed9812b4ce6b075e08f49fe" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
