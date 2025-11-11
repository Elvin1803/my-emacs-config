;;; early-init.el

(setq package-enable-at-startup nil)

;; minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)

;; Deactivate bell sound
(setq ring-bell-function 'ignore)

;; Prevent Emacs from doing backups
(setq make-backup-files nil)
(setq warning-minimum-level :error)
;;; early-init.el ends here
