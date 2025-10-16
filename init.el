(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

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
    cmake-ide
    nix-mode
    magit
    company
    lsp-mode
    lsp-ui
    ;; Text editing / Navigation
    avy
    move-text
    smartparens
    yasnippet
    ;; Misc
    dashboard
    helm
    neotree
    elcord
    ))

(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; Install missing packages
(dolist (package my/packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'elcord)
(elcord-mode)

(require 'org)
(org-babel-load-file "~/.emacs.d/config.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("95b0bc7b8687101335ebbf770828b641f2befdcf6d3c192243a251ce72ab1692"
     "aa545934ce1b6fd16b4db2cf6c2ccf126249a66712786dd70f880806a187ac0b"
     default))
 '(package-selected-packages
   '(all-the-icons avy cmake-ide company dashboard elcord git-gutter
                   git-gutter-fringe helm lsp-java lsp-ui magit
                   monokai-theme move-text neotree nix-mode
                   org-sticky-header page-break-lines projectile
                   rainbow-delimiters rust-analyzer rust-mode
                   smartparens standard-theme standard-themes
                   yasnippet))
 '(whitespace-display-mappings
   '((space-mark 32 [183] [46]) (space-mark 160 [164] [95])
     (tab-mark 9 [187 9] [92 9]))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
