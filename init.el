(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)

(setq my/packages
      '(
	;; Visual packages
	spacemacs-theme
	all-the-icons
	page-break-lines
	org-sticky-header
	rainbow-delimiters
	git-gutter
	git-gutter-fringe
	;; Code
	projectile
	magit
	company
	lsp-mode
	lsp-ui
	rust-mode
	;; Text editing / Navigation
	avy
	move-text
	smartparens
	yasnippet
	;; Misc
	dashboard
	helm
	neotree
	))

(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; Install missing packages
(dolist (package my/packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'org)
(org-babel-load-file "~/.emacs.d/config.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rust-mode rust-analyzer git-gutter-fringe git-gutter yasnippet spacemacs-theme smartparens rainbow-delimiters projectile page-break-lines org-sticky-header neotree move-text magit lsp-ui helm dashboard company avy all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
