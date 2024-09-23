(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(setq debug-on-error t)
(setq debug-on-quit t)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight.el with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Performance optimization
;; (use-package use-package-report
;;   :straight t
;;   :commands (use-package-report)
;;   :config
;;   (setq use-package-report-file (expand-file-name "use-package-report.el" user-emacs-directory)))

;; Use a larger gc-cons-threshold during initialization to speed up startup
(setq gc-cons-threshold (* 100 1024 1024))

;; Reset gc-cons-threshold after initialization
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 10 1024 1024))))

;; Define WS detection
(defvar is-wsl
  (or (getenv "WSLENV")
      (string-match-p "Microsoft" (shell-command-to-string "cat /proc/version"))))

;; Check environment and load appropriate configurations
(cond
 ;; X11 environment
 ((getenv "DISPLAY")
  (message "Running under X11")
  (if is-wsl
      (progn
        (message "X11/WSL"))
    (progn
      (message "X11")
      (require 'exwm)
      ;; Set the initial workspace number.
      (setq exwm-workspace-number 1)
      ;; Make class name the buffer name.
      (add-hook 'exwm-update-class-hook
		(lambda () (exwm-workspace-rename-buffer exwm-class-name)))
      ;; Global keybindings.
      (setq exwm-input-global-keys
	    `(([?\s-r] . exwm-reset) ;; s-r: Reset (to line-mode).
              ([?\s-w] . exwm-workspace-switch) ;; s-w: Switch workspace.
              ([?\s-d] . (lambda (cmd) ;; s-&: Launch application.
			   (interactive (list (read-shell-command "$ ")))
			   (start-process-shell-command cmd nil cmd)))
              ;; s-N: Switch to certain workspace.
              ,@(mapcar (lambda (i)
			  `(,(kbd (format "s-%d" i)) .
			    (lambda ()
                              (interactive)
                              (exwm-workspace-switch-create ,i))))
			(number-sequence 0 9))
	      ([?\C-\s-f] . (lambda () ;; s-C-f Launch firefox
			      (interactive)
			      (start-process-shell-command "firefox-bin" nil "firefox-bin")))
	      ([?\C-\s-d] . (lambda ()
			      (interactive)
			      (start-process-shell-command "discord" nil "discord")))
	      ))
      ;; Enable EXWM
      (exwm-enable)
      ;;(setq debug-on-error t)
      ;; (setq debug-on-quit t)
      ;;(setq edebug-all-forms t)
      ;;(exwm-systemtray-mode 1)

      (use-package fancy-battery
	:init
	(setq battery-update-interval 15)
	:config
	(fancy-battery-mode)
	(setq fancy-battery-show-percentage t)  ;; Show battery percentage
	(setq fancy-battery-show-time t)  ;; Show remaining time
	)

      (setq exwm-randr-workspace-monitor-plist '(0 "VGA1"))
      (add-hook 'exwm-randr-screen-change-hook
		(lambda ()
		  (start-process-shell-command
		   "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
      (exwm-randr-mode 1)
      )
    )
  )

 ;; Wayland environment
 ((getenv "WAYLAND_DISPLAY")
  (message "Running under Wayland")
  (if is-wsl
      (progn
        (message "Wayland/WSL")
					;(load "~/.emacs.d/wayland-wsl-config.el")
	)
    (progn
      (message "Wayland")
					;(load "~/.emacs.d/wayland-config.el")
      ))))
 ;; Load core Emacs configuration
 (load "~/.newemacs.d/basics.el")

 ;; Load package management configuration
 (load "~/.newemacs.d/modules/packages.el")

 (load "~/.newemacs.d/modules/org.el")

(add-hook 'emacs-startup-hook #'efs/display-startup-time)
(use-package-report)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(git-gutter-fringe smartparens lsp-ui vdiff json-mode org-roam magit rustic yaml-mode xterm-color w3m vterm toml ranger rainbow-delimiters projectile markdown-mode lv latex-preview-pane json-snatcher git-gutter fringe-helper flycheck dashboard dash compat base16-theme all-the-icons ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:strike-through t)))))

(with-current-buffer "*Messages*"
  (write-region (point-min) (point-max) "~/.newemacs.d/startup-errors.log"))
