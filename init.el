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

(add-hook 'after-init-hook 'global-undo-tree-mode) ;;undo-tree mode

;;having some trouble with fonts in terminal, choose a monospaced
(if (equal (display-graphic-p) 'false) (set-default-font "Inconsolata-12")
)

(setq initial-scratch-message "") ;;get rid of annoying scratch message

;;auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;;python
(add-to-list 'load-path "~/.emacs.d/pymacs") ;;mv experiment
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;;get python indentation right: - MV temporarily canceled this to see what effect would be
;;(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'ne;;wline-and-indent)))

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
(toggle-fullscreen)

;;select color theme
(color-theme-initialize)
(color-theme-solarized-dark)
