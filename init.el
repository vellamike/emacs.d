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
        color-theme-solarized
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

;;monospaced font
(if (equal (display-graphic-p) 'false) (set-default-font "Inconsolata-12")
)

(setq initial-scratch-message "") ;;get rid of annoying scratch message

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
(color-theme-initialize)
(color-theme-solarized-dark)

;;python-mode indentation
;;this is a matter of taste, when I hit ENTR and I'm in a method I
;;want the level of indentation to be preserved rather than having to
;;hit TAB agian.
(add-hook 'python-mode-hook
          (lambda ()
             (define-key python-mode-map "\r" 'newline-and-indent)))
