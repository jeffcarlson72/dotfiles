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
 '(diff-switches "-u")
 '(dired-listing-switches "-al --group-directories-first")
 '(electric-pair-mode t)
 '(electric-quote-mode nil)
 '(kill-do-not-save-duplicates t)
 '(line-number-mode t)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case nil)
 '(ls-lisp-use-string-collate nil)
 '(org-startup-indented t)
 '(package-selected-packages (quote (apache-mode
				     beacon
				     cfengine-mode
				     color-theme-modern
				     diminish
				     helm
				     htmlize
				     json-mode
				     magit
				     markdown-mode
				     markdown-preview-mode
				     mmm-jinja2
				     salt-mode
				     ssh-config-mode
				     terraform-mode
				     try
				     use-package
				     yaml-mode
				     yasnippet
				     yasnippet-snippets)))
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

(add-hook 'awk-mode-hook        'hs-minor-mode)
(add-hook 'bibtex-mode-hook     'hs-minor-mode)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'c-mode-hook          'hs-minor-mode)
(add-hook 'c++-mode-hook        'hs-minor-mode)
(add-hook 'css-mode-hook        'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'fortran-mode-hook    'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'js-mode-hook         'hs-minor-mode)
(add-hook 'json-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'php-mode-hook        'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'ruby-mode-hook       'hs-minor-mode)
(add-hook 'sed-mode-hook        'hs-minor-mode)
(add-hook 'scheme-mode-hook     'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'tcl-mode-hook        'hs-minor-mode)
(add-hook 'vhdl-mode-hook       'hs-minor-mode)

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
(with-eval-after-load "org"
  (setq org-structure-template-alist
	(cons '("sawk"
		"#+BEGIN_SRC awk\n?\n#+END_SRC"
		"<src lang=\"awk\">\n?\n</src>")
              org-structure-template-alist))
  (setq org-structure-template-alist
	(cons '("sed"
		"#+BEGIN_SRC sed\n?\n#+END_SRC"
		"<src lang=\"sed\">\n?\n</src>")
              org-structure-template-alist))
  (setq org-structure-template-alist
	(cons '("sc"
		"#+BEGIN_SRC c\n?\n#+END_SRC"
		"<src lang=\"c\">\n?\n</src>")
	      org-structure-template-alist))
  (setq org-structure-template-alist
	(cons '("sel"
		"#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"
		"<src lang=\"emacs-lisp\">\n?\n</src>")
              org-structure-template-alist))
  (setq org-structure-template-alist
	(cons '("sjs"
		"#+BEGIN_SRC javascript\n?\n#+END_SRC"
		"<src lang=\"javascript\">\n?\n</src>")
	      org-structure-template-alist))
  (setq org-structure-template-alist
	(cons '("spl"
		"#+BEGIN_SRC perl\n?\n#+END_SRC"
		"<src lang=\"perl\">\n?\n</src>")
	      org-structure-template-alist))
  (setq org-structure-template-alist
	(cons '("sphp"
		"#+BEGIN_SRC php\n?\n#+END_SRC"
		"<src lang=\"php\">\n?\n</src>")
	      org-structure-template-alist))
  (setq org-structure-template-alist
	(cons '("spy"
		"#+BEGIN_SRC python\n?\n#+END_SRC"
		"<src lang=\"python\">\n?\n</src>")
	      org-structure-template-alist))
  (setq org-structure-template-alist
	(cons '("ssh"
		"#+BEGIN_SRC shell\n?\n#+END_SRC"
		"<src lang=\"shell\">\n?\n</src>")
	      org-structure-template-alist)))
;; org-mode end

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  ;; You can run the command 'package-refresh-contents' with M-x pa-r- RET
  (package-install 'use-package))

(use-package all-the-icons
  :ensure t)
;; M-x all-the-icons-install-fonts

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
;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'jedi:setup))

(use-package diminish
  :ensure t
  :config
  (require 'diminish))

(use-package dired
  :config
  (setenv "LC_COLLATE" "C")
  (require 'ls-lisp))

(use-package helm
  :ensure t
  :diminish
  :config
  (require 'helm-config)
  (helm-mode))

(use-package hideshow
  :diminish 'hs-minor-mode)

(use-package htmlize
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package magit
  :ensure t
  :config
  (when (memq system-type '(windows-nt ms-dos)) ;; fml
    (setenv "GIT_SSH" "plink.exe")))

(use-package markdown-mode
  :ensure t)
(use-package markdown-preview-mode
  :ensure t)

(use-package org-download
  :ensure t)

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))

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

(use-package yaml-mode
  :ensure t
  :config
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
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

;; Last thing, load local config
(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))
;; For example:
;; (setq user-mail-address "username@domain.tld")

;; Local Variables:
;; mode: emacs-lisp
;; End:
