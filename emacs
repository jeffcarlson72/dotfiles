;;
;;    ___ _ __ ___   __ _  ___ ___ 
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \ 
;; (_)___|_| |_| |_|\__,_|\___|___/
;;

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and
  ;; MELPA Stable as desired
  (add-to-list 'package-archives
	       (cons "melpa"
		     (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives
	       (cons "melpa-stable"
		     (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives
	       (cons "nongnu"
		     (concat proto "://elpa.nongnu.org/nongnu")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
		 (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

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
 '(company-require-match nil)
 '(diff-switches "-u")
 '(dired-listing-switches "-al --group-directories-first")
 '(display-fill-column-indicator-column 80)
 '(electric-pair-mode t)
 '(electric-quote-mode nil)
 '(exec-path (append exec-path '("/usr/local/bin")))
 '(global-hi-lock-mode 1) ;; https://www.emacswiki.org/emacs/HiLock
 '(isearch-lazy-count t)
 '(kill-do-not-save-duplicates t)
 ;; '(lazy-count-prefix-format "(%s/%s) ")
 ;; '(lazy-count-suffix-format nil)
 '(line-number-mode t)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case nil)
 '(ls-lisp-use-string-collate nil)
 '(org-startup-indented t)
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((c-file-style . stroustrup))))
 '(show-paren-mode t)
 '(ssh-config-mode-indent 4)
 '(transient-mark-mode t)
 '(user-full-name "Jeff Carlson")
 '(visible-bell t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Cascadia Code"
		:foundry "outline"
		:slant normal
		:weight normal
		:height 120
		:width normal)))))

(blink-cursor-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(scroll-bar-mode 0)
(windmove-default-keybindings)
(when (>= emacs-major-version 27)
  (global-display-fill-column-indicator-mode 1))
(when (>= emacs-major-version 19)
  (xterm-mouse-mode 1))

;; Shift-PageUp or Shift-PageDown makes the buffer scroll by one line
;; at a time.  Modified from example by Emacs Elements.
(global-set-key (kbd "S-<prior>")
		(kbd "C-u 1 M-v"))
(global-set-key (kbd "S-<next>")
		(kbd "C-u 1 C-v"))

(setq-default abbrev-mode t)

(add-hook 'awk-mode-hook        'hs-minor-mode)
(add-hook 'bibtex-mode-hook     'hs-minor-mode)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'c-mode-hook          'hs-minor-mode)
(add-hook 'c++-mode-hook        'hs-minor-mode)
(add-hook 'cfengine3-mode-hook	'hs-minor-mode)
(add-hook 'css-mode-hook        'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'fortran-mode-hook    'hs-minor-mode)
(add-hook 'go-mode-hook         'hs-minor-mode)
(add-hook 'html-mode-hook       'hs-minor-mode)
(add-hook 'idl-mode-hook        'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'js-mode-hook         'hs-minor-mode)
(add-hook 'json-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'objc-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'php-mode-hook        'hs-minor-mode)
(add-hook 'pike-mode-hook       'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'ruby-mode-hook       'hs-minor-mode)
(add-hook 'sed-mode-hook        'hs-minor-mode)
(add-hook 'scheme-mode-hook     'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'tcl-mode-hook        'hs-minor-mode)
(add-hook 'terraform-mode-hook  'hs-minor-mode)
(add-hook 'vhdl-mode-hook       'hs-minor-mode)
(add-hook 'yaml-mode-hook       'hs-minor-mode)

(require 'tramp)
(if (memq system-type '(windows-nt ms-dos)) ;; fml
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))
(add-to-list 'tramp-default-proxies-alist
	     '(nil "\\`root\\'" (concat "/" tramp-default-method ":%h:")))
(add-to-list 'tramp-default-proxies-alist
	     '((regexp-quote (system-name)) nil nil))
(tramp-set-completion-function
 "ssh" '((tramp-parse-shosts  "/etc/ssh/ssh_known_hosts")
	 (tramp-parse-shosts  "~/.ssh/known_hosts")
	 (tramp-parse-sconfig "/etc/ssh/ssh_config")
	 (tramp-parse-sconfig "~/.ssh/config")
	 (tramp-parse-hosts   "/etc/hosts")))

(add-to-list 'auto-mode-alist '("/bash-fc" . shell-script-mode))
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)

(require 'server)
(unless (server-running-p)
  (server-start))

;; org-mode begin
(require 'org-tempo) ;; enable <s TAB
(setq org-structure-template-alist
      '(("awk"  . "src awk\n")
	("cc"   . "src c\n")
	("cfe"  . "src cfengine3\n")
	("cl"   . "src lisp\n")
	("css"  . "src css\n")
	("el"   . "src emacs-lisp\n")
	("go"   . "src go\n")
	("html" . "src html\n")
	("json" . "src json\n")
	("m4"   . "src m4\n")
	("php"  . "src php\n")
	("pl"   . "src perl\n")
	("py"   . "src python\n")
	("rb"   . "src ruby\n")
	("rs"   . "src rust\n")
	("sed"  . "src sed\n")
	("sh"   . "src shell\n")
	("sql"  . "src sql\n")
	("tex"  . "src latex\n")
	("xml"  . "src xml\n")
	("yml"  . "src yaml\n")
	("a"    . "export ascii\n")
	("c"    . "center\n")
	("C"    . "comment\n")
	("e"    . "example\n")
	("E"    . "export\n")
	("h"    . "export html\n")
	("l"    . "export latex\n")
	("q"    . "quote\n")
	("s"    . "src\n")
	("v"    . "verse\n")))
;; org-mode end

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  ;; You can run the command 'package-refresh-contents' with M-x pa-r- RET
  (package-install 'use-package))

(use-package all-the-icons
  :ensure t)
;; M-x all-the-icons-install-fonts
(use-package all-the-icons-dired
  :ensure t
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package apache-mode
  :ensure t)

(use-package atomic-chrome
  ;; Use Atomic Chrome for Chrome or GhostText for Mozilla
  :ensure t
  :config
  (atomic-chrome-start-server))
(setq atomic-chrome-buffer-open-style 'frame)

(use-package beacon
  :ensure t
  :config
  (beacon-mode t))

(use-package cfengine
  ;; https://raw.github.com/cfengine/core/master/contrib/cfengine.el
  ;; Installs automatically with cfengine package
  :config
  (autoload 'cfengine-mode "cfengine" "cfengine editing" t)
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (add-to-list 'auto-mode-alist '("\\.cf\\'" . cfengine-mode)))

(use-package color-theme-modern
  :ensure t
  :config
  (load-theme 'classic t t)
  (enable-theme 'classic))

(use-package company
  :ensure t
  :diminish 'company-mode
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t))
(use-package company-jedi
  :ensure t
  :config
  (require 'company)
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:install-server)
  (add-hook 'python-mode-hook 'python-mode-company-init)
  (add-hook 'python-mode-hook
	    (lambda ()
	      (add-to-list 'company-backends '(company-jedi
					       company-files
					       company-etags
					       company-dabbrev-code)))))
(use-package company-shell
  :ensure t
  :config
  (require 'company)
  (add-hook 'shell-mode-hook 'shell-mode-company-init))

(use-package diminish
  :ensure t
  :config
  (require 'diminish))

(use-package dired
  :config
  (setenv "LC_COLLATE" "C")
  (require 'ls-lisp))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'html-mode-hook 'emmet-mode))

(use-package helm
  :ensure t
  :bind
  ("C-x C-b" . 'helm-buffers-list)
  ("C-x b"   . 'helm-mini)
  ("C-x C-f" . 'helm-find-files)
  ("M-x"     . 'helm-M-x)
  :diminish
  :pin nongnu
  :config
  (require 'helm-config)
  (helm-mode))

(use-package hideshow
  :diminish 'hs-minor-mode)

(use-package htmlize
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package jinja2-mode
  :ensure t)

(use-package json-mode
  :ensure t
  :hook (json-mode . (lambda ()
		       (make-local-variable 'js-indent-level)
		       (setq js-indent-level 2))))

(use-package magit
  :ensure t
  :config
  (when (memq system-type '(windows-nt ms-dos)) ;; fml
    (setenv "GIT_SSH" "plink.exe")))

(use-package markdown-mode
  :ensure t)
(use-package markdown-preview-mode
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("M-s m m" . mc/edit-lines)
	 ("M-s m a" . mc/mark-all-like-this)
	 ("M-s m n" . mc/mark-next-like-this)
	 ("M-s m s" . mc/skip-to-next-like-this)
	 ("M-s m u" . mc/unmark-next-like-this)))

(use-package mustache
  :ensure t)
(use-package mustache-mode
  :ensure t)

(use-package org-download
  :ensure t)

(use-package php-mode
  :ensure t)

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

(use-package rust-mode
  :ensure t)

(use-package sed-mode
  :ensure t)

(use-package ssh-config-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))

(use-package terraform-mode
  :ensure t)

(use-package treemacs
  :ensure t
  :bind
  (:map global-map
	([f8] . treemacs)))

(use-package try
  :ensure t)

(use-package vterm
  :ensure t)
(use-package eshell-vterm
  :ensure t
  :after eshell
  :config
  (eshell-vterm-mode))
(defalias 'eshell/v 'eshell-exec-visual)

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 0.3))

(use-package yaml-mode
  :ensure t
  :config
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
	    '(lambda ()
	       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package yasnippet
  :ensure t
  :diminish 'yas-minor-mode
  :config
  (require 'yasnippet)
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt)))
(use-package yasnippet-snippets
  :ensure t)

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

(defface hi-red-on-black
  '((((background dark)) (:background "black" :foreground "red"))
    (t (:background "black")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-green-on-black
  '((((background dark)) (:background "black" :foreground "green"))
    (t (:background "black")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-yellow-on-black
  '((((background dark)) (:background "black" :foreground "yellow"))
    (t (:background "black")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-blue-on-black
  '((((background dark)) (:background "black" :foreground "blue"))
    (t (:background "black")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-violet-on-black
  '((((background dark)) (:background "black" :foreground "violet"))
    (t (:background "black")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-cyan-on-black
  '((((background dark)) (:background "black" :foreground "cyan"))
    (t (:background "black")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-white-on-black
  '((((background dark)) (:background "black" :foreground "white"))
    (t (:background "black")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

;; Last thing, load local config
(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))
;; For example:
;; (setq user-mail-address "username@domain.tld")

;; Local Variables:
;; mode: emacs-lisp
;; End:

;; apache-mode
;; bar-cursor
;; bm
;; boxquote
;; browse-kill-ring
;; color-theme-modern
;; csv-mode
;; diminish
;; eproject
;; folding
;; graphviz-dot-mode
;; htmlize
;; initsplit
;; markdown-mode
;; mutt-alias
;; muttrc-mode
;; pod-mode
;; session
;; tabbar
