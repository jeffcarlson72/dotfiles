;;
;;    ___ _ __ ___   __ _  ___ ___ 
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \ 
;; (_)___|_| |_| |_|\__,_|\___|___/
;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(diff-switches "-u")
 '(electric-indent-mode t)
 '(electric-pair-mode t)
 '(package-archives
   (quote
    (("gnu"       . "http://elpa.gnu.org/packages/")
     ("melpa"     . "https://melpa.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(package-enable-at-startup t)
 '(package-selected-packages (quote (apache-mode
				     helm
				     helm-ebdb
				     json-mode
				     magit
				     markdown-mode
				     markdown-preview-mode
				     ssh-config-mode
				     terraform-mode
				     try
				     yaml-mode
				     yasnippet
				     yasnippet-snippets)))
 '(safe-local-variable-values (quote ((c-file-style . stroustrup))))
 '(ssh-config-mode-indent 4)
 '(show-paren-mode t)
 '(tramp-default-method "ssh")
 '(transient-mark-mode t)
 '(user-full-name "Jeff Carlson")
 '(user-mail-address "jeff ultimateevil org")
 '(visible-bell t))

;(require 'helm)
(require 'helm-config)
(helm-mode 1)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'bibtex-mode-hook     'hs-minor-mode)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'c-mode-hook          'hs-minor-mode)
(add-hook 'c++-mode-hook        'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'fortran-mode-hook    'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'js-mode-hook         'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'php-mode-hook        'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'ruby-mode-hook       'hs-minor-mode)
(add-hook 'scheme-mode-hook     'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'tcl-mode-hook        'hs-minor-mode)
(add-hook 'vhdl-mode-hook       'hs-minor-mode)

(autoload 'ssh-config-mode "ssh-config-mode" t)
(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

(require 'tramp)
(setq tramp-default-method "ssh")
(add-to-list 'tramp-default-proxies-alist
	     '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
	     '((regexp-quote (system-name)) nil nil))
(tramp-set-completion-function "ssh"
			       '((tramp-parse-shosts  "/etc/ssh/ssh_known_hosts")
				 (tramp-parse-shosts  "~/.ssh/known_hosts")
				 (tramp-parse-sconfig "/etc/ssh/ssh_config")
				 (tramp-parse-sconfig "~/.ssh/config")
				 (tramp-parse-hosts   "/etc/hosts")))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/color-theme/")
(require 'color-theme)
(color-theme-initialize)
(color-theme-classic)

(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'auto-mode-alist '("/bash-fc" . shell-script-mode))
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)

(autoload 'cfengine-mode "cfengine" "cfengine editing" t)
(add-to-list 'auto-mode-alist '("\\.cf\\'" . cfengine-mode))

(add-to-list 'load-path "~/.elisp")
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-0.13.0")
(require 'yasnippet)
(yas-global-mode 1)

(org-babel-do-load-languages 'org-babel-load-languages
			     '((C		. nil)
			       (D		. nil)
			       (F90		. nil)
			       (J		. nil)
			       (R		. nil)
			       (abc		. nil)
			       (asymptote	. nil)
			       (awk		. t)
			       (axiom		. nil)
			       (browser		. nil)
			       (calc		. nil)
			       (clojure		. nil)
			       (comint		. nil)
			       (coq		. nil)
			       (cpp		. nil)
			       (css		. t)
			       (cypher		. nil)
			       (ditaa		. nil)
			       (dot		. nil)
			       (ebnf		. nil)
			       (elixir		. nil)
			       (emacs-lisp	. t)
			       (eukleides	. nil)
			       (fomus		. nil)
			       (forth		. nil)
			       (gnuplot		. nil)
			       (groovy		. nil)
			       (haskell		. nil)
			       (http		. nil)
			       (io		. nil)
			       (ipython		. nil)
			       (java		. nil)
			       (js		. t)
			       (julia		. nil)
			       (kotlin		. nil)
			       (latex		. t)
			       (ledger		. nil)
			       (lfe		. nil)
			       (lisp		. t)
			       (ly		. nil)
			       (makefile	. t)
			       (mathematica	. nil)
			       (mathomatic	. nil)
			       (matlab		. nil)
			       (max		. nil)
			       (mongo		. nil)
			       (mscgen		. nil)
			       (ocaml		. nil)
			       (octave		. nil)
			       (org		. t)
			       (oz		. nil)
			       (perl		. t)
			       (picolisp	. nil)
			       (plantuml	. nil)
			       (processing	. nil)
			       (prolog		. nil)
			       (python		. t)
			       (rec		. nil)
			       (ruby		. t)
			       (sass		. nil)
			       (scala		. nil)
			       (scheme		. nil)
			       (screen		. t)
			       (sed		. t)
			       (shell		. t)
			       (shen		. nil)
			       (sml		. nil)
			       (spad		. nil)
			       (sql		. t)
			       (sqlite		. t)
			       (stan		. nil)
			       (stata		. nil)
			       (tcl		. nil)
			       (translate	. nil)
			       (typescript	. nil)
			       (vala		. nil)))

(put 'narrow-to-region 'disabled nil)

(speedbar)

(require 'server)
(unless (server-running-p)
  (server-start))
